(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module Rrdd = Rrd_client.Client
module Date = Clock.Date
open Monitor_types
open Xapi_database.Db_filter_types
open Network

module D = Debug.Make (struct let name = "monitor_master" end)

open D

let update_configuration_from_master () =
  Server_helpers.exec_with_new_task "update_configuration_from_master"
    (fun __context ->
      let oc =
        Db.Pool.get_other_config ~__context ~self:(Helpers.get_pool ~__context)
      in
      let new_use_min_max =
        List.mem_assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc
        && List.assoc Xapi_globs.create_min_max_in_new_VM_RRDs oc = "true"
      in
      log_and_ignore_exn (fun () -> Rrdd.update_use_min_max new_use_min_max) ;
      let carrier =
        List.mem_assoc Xapi_globs.pass_through_pif_carrier_key oc
        && List.assoc Xapi_globs.pass_through_pif_carrier_key oc = "true"
      in
      if !Xapi_globs.pass_through_pif_carrier <> carrier then
        debug "Updating pass_through_pif_carrier: New value=%b" carrier ;
      Xapi_globs.pass_through_pif_carrier := carrier
  )

let get_pciids vendor device =
  (* FIXME : put a lazy cache *)
  let v, d = Pciutil.parse vendor device in
  ( (match v with None -> "" | Some x -> x)
  , match d with None -> "" | Some x -> x
  )

let set_pif_metrics ~__context ~self ~vendor ~device ~carrier ~speed ~duplex
    ~pcibuspath pmr =
  (* don't update & and reread pciids if db already contains same value *)
  if
    pmr.API.pIF_metrics_vendor_id <> vendor
    || pmr.API.pIF_metrics_device_id <> device
  then (
    let vendor_str, device_str = get_pciids vendor device in
    Db.PIF_metrics.set_vendor_id ~__context ~self ~value:vendor ;
    Db.PIF_metrics.set_device_id ~__context ~self ~value:device ;
    Db.PIF_metrics.set_vendor_name ~__context ~self ~value:vendor_str ;
    Db.PIF_metrics.set_device_name ~__context ~self ~value:device_str
  ) ;
  if pmr.API.pIF_metrics_carrier <> carrier then
    Db.PIF_metrics.set_carrier ~__context ~self ~value:carrier ;
  if pmr.API.pIF_metrics_speed <> speed then
    Db.PIF_metrics.set_speed ~__context ~self ~value:speed ;
  if pmr.API.pIF_metrics_duplex <> duplex then
    Db.PIF_metrics.set_duplex ~__context ~self ~value:duplex ;
  if pmr.API.pIF_metrics_pci_bus_path <> pcibuspath then
    Db.PIF_metrics.set_pci_bus_path ~__context ~self ~value:pcibuspath ;
  Db.PIF_metrics.set_last_updated ~__context ~self ~value:(Date.now ())

(* Note that the following function is actually called on the slave most of the
 * time now but only when the PIF information changes. *)
let update_pifs ~__context host pifs =
  match List.length pifs with
  | 0 ->
      ()
  | _ ->
      (* Fetch all physical and bond PIFs from DB. *)
      let db_pifs =
        Db.PIF.get_records_where ~__context
          ~expr:
            (And
               ( Eq (Field "host", Literal (Ref.string_of host))
               , Or
                   ( Eq (Field "physical", Literal "true")
                   , Not (Eq (Field "bond_master_of", Literal "()"))
                   )
               )
            )
      in
      (* Iterate over them, and spot and update changes. *)
      List.iter
        (fun (pifdev, pifrec) ->
          try
            let pif_stats =
              List.find (fun p -> p.pif_name = pifrec.API.pIF_device) pifs
            in
            let carrier = pif_stats.pif_carrier in
            let speed = Int64.of_int pif_stats.pif_speed in
            let duplex =
              match pif_stats.pif_duplex with
              | Network_interface.Duplex_full ->
                  true
              | Network_interface.Duplex_half ->
                  false
              | Network_interface.Duplex_unknown ->
                  false
            in
            let vendor = pif_stats.pif_vendor_id in
            let device = pif_stats.pif_device_id in
            let pcibuspath = pif_stats.pif_pci_bus_path in
            (* 1. Update corresponding VIF carrier flags *)
            if !Xapi_globs.pass_through_pif_carrier then (
              try
                let open Xapi_xenops_queue in
                let dbg = Context.string_of_task __context in
                let vifs_on_local_bridge network =
                  try
                    Db.Network.get_bridge ~__context ~self:network
                    |> Net.Bridge.get_interfaces dbg
                    |> List.filter_map vif_device_of_string
                  with _ -> []
                in
                let set_carrier (domid, devid) =
                  let expr =
                    And
                      ( Eq (Field "resident_on", Literal (Ref.string_of host))
                      , Eq (Field "domid", Literal (string_of_int domid))
                      )
                  in
                  match Db.VM.get_refs_where ~__context ~expr with
                  | [] ->
                      ()
                      (* This may be a VM that is not managed by us: ignore *)
                  | vm :: _ ->
                      let vm_uuid = Db.VM.get_uuid ~__context ~self:vm in
                      let vif_id = (vm_uuid, string_of_int devid) in
                      let queue_name =
                        Xapi_xenops_queue.queue_of_vm ~__context ~self:vm
                      in
                      let module Xenopsd = (val make_client queue_name : XENOPS)
                      in
                      Xenopsd.VIF.set_carrier dbg vif_id carrier
                      |> Xapi_xenops.sync __context queue_name
                in
                (* Go from physical interface -> bridge -> vif devices.
                 * Do this for the physical network and any VLANs/tunnels on top of it. *)
                let vlan_networks =
                  List.map
                    (fun vlan ->
                      let vlan_master =
                        Db.VLAN.get_untagged_PIF ~__context ~self:vlan
                      in
                      Db.PIF.get_network ~__context ~self:vlan_master
                    )
                    pifrec.API.pIF_VLAN_slave_of
                in
                let tunnel_networks =
                  List.map
                    (fun tunnel ->
                      let access_pif =
                        Db.Tunnel.get_access_PIF ~__context ~self:tunnel
                      in
                      Db.PIF.get_network ~__context ~self:access_pif
                    )
                    pifrec.API.pIF_tunnel_transport_PIF_of
                in
                (pifrec.API.pIF_network :: vlan_networks) @ tunnel_networks
                |> List.concat_map vifs_on_local_bridge
                |> List.iter set_carrier
              with e ->
                log_backtrace e ;
                error "Failed to update VIF carrier flags for PIF: %s"
                  (ExnHelper.string_of_exn e)
            ) ;
            (* 2. Update database *)
            let metrics =
              (* If PIF metrics don't exist then create them *)
              if Db.is_valid_ref __context pifrec.API.pIF_metrics then
                pifrec.API.pIF_metrics
              else
                let ref = Ref.make () in
                Db.PIF_metrics.create ~__context ~ref
                  ~uuid:(Uuidx.to_string (Uuidx.make ()))
                  ~carrier:false ~device_name:"" ~vendor_name:"" ~device_id:""
                  ~vendor_id:"" ~speed:0L ~duplex:false ~pci_bus_path:""
                  ~io_read_kbs:0. ~io_write_kbs:0. ~last_updated:Date.epoch
                  ~other_config:[] ;
                Db.PIF.set_metrics ~__context ~self:pifdev ~value:ref ;
                ref
            in
            let pmr = Db.PIF_metrics.get_record ~__context ~self:metrics in
            set_pif_metrics ~__context ~self:metrics ~vendor ~device ~carrier
              ~speed ~duplex ~pcibuspath pmr
          with Not_found -> ()
        )
        db_pifs
