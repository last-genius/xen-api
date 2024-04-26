open Rrdd_shared
open Rrd
open Ds

module D = Debug.Make (struct let name = "rrdd_monitor" end)

open D

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list
    (List.flatten
       (List.map
          (fun (n, ns) ->
            if ns > 1 && use_min_max then
              [
                Rrd.rra_create Rrd.CF_Average n ns 1.0
              ; Rrd.rra_create Rrd.CF_Min n ns 1.0
              ; Rrd.rra_create Rrd.CF_Max n ns 1.0
              ]
            else
              [Rrd.rra_create Rrd.CF_Average n ns 0.5]
          )
          timescales
       )
    )

let step = 5L

(** Create a rrd *)
let create_fresh_rrd use_min_max dss =
  let rras = create_rras use_min_max in
  let dss =
    Array.of_list
      (List.filter_map
         (fun (ts, ds) ->
           if ds.ds_default then
             Some
               ( ts
               , Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max
                   ~min:ds.ds_min Rrd.VT_Unknown
               )
           else
             None
         )
         dss
      )
  in
  Rrd.rrd_create dss rras step

let merge_new_dss rrd dss =
  let should_enable_ds _ (_, ds) =
    !Rrdd_shared.enable_all_dss || ds.ds_default
  in
  let enabled_dss = StringMap.filter should_enable_ds dss in
  let current_dss = Rrd.ds_names rrd |> StringSet.of_list in
  let new_dss =
    StringMap.filter
      (fun ds_name _ -> not (StringSet.mem ds_name current_dss))
      enabled_dss
  in
  (* fold on Map is not tail-recursive, but the depth of the stack should be
     log of the number of entries at worst, so this should be alright.
     Previous conversions to List are also not tail-recursive with identical
     stack depth *)
  StringMap.fold
    (fun _key (timestamp, ds) rrd ->
      rrd_add_ds rrd timestamp
        (Rrd.ds_create ds.ds_name ds.Ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)
    )
    new_dss rrd

module OwnerMap = Map.Make (struct
  type t = ds_owner

  let compare a b =
    match (a, b) with
    | Host, Host ->
        0
    | Host, _ | VM _, SR _ ->
        -1
    | _, Host | SR _, VM _ ->
        1
    | VM a, VM b | SR a, SR b ->
        String.compare a b
end)

let owner_to_string () = function
  | Host ->
      "host"
  | VM uuid ->
      "VM " ^ uuid
  | SR uuid ->
      "SR " ^ uuid

(** Updates all of the hosts rrds. We are passed a list of uuids that is used as
    the primary source for which VMs are resident on us. When a new uuid turns
    up that we haven't got an RRD for in our hashtbl, we create a new one. When
    a uuid for which we have an RRD for doesn't appear to have any stats this
    update, we assume that the domain has gone and we stream the RRD to the
    master. We also have a list of the currently rebooting VMs to ensure we
    don't accidentally archive the RRD. *)
let update_rrds dss uuid_domids paused_vms =
  let uuid_domids = List.to_seq uuid_domids |> StringMap.of_seq in
  let paused_vms = List.to_seq paused_vms |> StringSet.of_seq in
  let consolidate all ((owner, (_timestamp, ds)) as v) =
    let add_ds_to = StringMap.add ds.ds_name v in
    let merge = function
      | None ->
          Some (add_ds_to StringMap.empty)
      | Some dss ->
          Some (add_ds_to dss)
    in
    OwnerMap.update owner merge all
  in
  let dss = Seq.fold_left consolidate OwnerMap.empty dss in

  (* Here we do the synchronising between the dom0 view of the world and our
     Hashtbl. By the end of this execute block, the Hashtbl correctly represents
     the world *)
  Xapi_stdext_threads.Threadext.Mutex.execute mutex (fun _ ->
      let out_of_date, by_how_much =
        let reading_timestamp = Unix.gettimeofday () in
        match !host_rrd with
        | None ->
            (false, 0.)
        | Some rrdi ->
            ( rrdi.rrd.Rrd.last_updated > reading_timestamp
            , abs_float (reading_timestamp -. rrdi.rrd.Rrd.last_updated)
            )
      in
      if out_of_date then
        error
          "Clock just went backwards by %.0f seconds: RRD data may now be \
           unreliable"
          by_how_much ;
      let process_vm vm_uuid named_updates
          (dss : (float * Ds.ds) Rrd.StringMap.t) =
        match StringMap.find_opt vm_uuid uuid_domids with
        | Some domid -> (
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt vm_rrds vm_uuid with
          | Some rrdi ->
              let rrd = merge_new_dss rrdi.rrd dss in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid} ;
              (* CA-34383: Memory updates from paused domains serve no useful
                 purpose. During a migrate such updates can also cause undesirable
                 discontinuities in the observed value of memory_actual. Hence, we
                 ignore changes from paused domains: *)
              if not (StringSet.mem vm_uuid paused_vms) then (
                Rrd.ds_update_named rrd ~new_domid:(domid <> rrdi.domid)
                  named_updates ;
                rrdi.dss <- dss ;
                rrdi.domid <- domid
              )
          | None ->
              debug "%s: Creating fresh RRD for VM uuid=%s" __FUNCTION__ vm_uuid ;
              let dss_list =
                StringMap.to_seq dss |> Seq.map snd |> List.of_seq
              in
              let rrd = create_fresh_rrd !use_min_max dss_list in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid}
        )
        | None ->
            info "%s: VM uuid=%s is not resident in this host, ignoring rrds"
              __FUNCTION__ vm_uuid
      in
      let process_sr sr_uuid named_updates dss =
        try
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt sr_rrds sr_uuid with
          | Some rrdi ->
              let rrd = merge_new_dss rrdi.rrd dss in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid= 0} ;
              Rrd.ds_update_named rrd ~new_domid:false named_updates ;
              rrdi.dss <- dss ;
              rrdi.domid <- 0
          | None ->
              debug "%s: Creating fresh RRD for SR uuid=%s" __FUNCTION__ sr_uuid ;
              let dss_list =
                StringMap.to_seq dss |> Seq.map snd |> List.of_seq
              in
              let rrd = create_fresh_rrd !use_min_max dss_list in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid= 0}
        with _ -> log_backtrace ()
      in
      let process_host named_updates dss =
        match !host_rrd with
        | None ->
            debug "%s: Creating fresh RRD for localhost" __FUNCTION__ ;
            let dss_list = StringMap.to_seq dss |> Seq.map snd |> List.of_seq in
            let rrd = create_fresh_rrd true dss_list in
            (* Always always create localhost rrds with min/max enabled *)
            host_rrd := Some {rrd; dss; domid= 0}
        | Some rrdi ->
            (*let dss_list = StringMap.to_seq dss |> Seq.map snd |> List.of_seq in*)
            rrdi.dss <- dss ;
            let rrd = merge_new_dss rrdi.rrd dss in
            host_rrd := Some {rrd; dss; domid= 0} ;
            Rrd.ds_update_named rrd ~new_domid:false named_updates
      in
      let process_dss ds_owner dss =
        (* the first parameter and ds.ds_name are equivalent *)
        let to_named_updates (_, (timestamp, ds)) =
          (timestamp, ds.ds_value, ds.ds_pdp_transform_function)
        in

        let named_updates = StringMap.map to_named_updates dss in
        let dss = StringMap.map snd dss in
        match ds_owner with
        | Host ->
            process_host named_updates dss
        | VM uuid ->
            process_vm uuid named_updates dss
        | SR uuid ->
            process_sr uuid named_updates dss
      in
      OwnerMap.iter process_dss dss
  )
