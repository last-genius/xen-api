open Rrdd_shared
open Rrd
open Ds

module D = Debug.Make (struct let name = "rrdd_monitor" end)

open D

let create_rras use_min_max =
  (* Create archives of type min, max and average and last *)
  Array.of_list
    (List.concat_map
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

let step = 5L

(** Create a rrd *)
let create_fresh_rrd use_min_max dss uid timestamp =
  let rras = create_rras use_min_max in
  let dss =
    Array.of_list
      (List.filter_map
         (fun ds ->
           if ds.ds_default then
             Some
               (Rrd.ds_create ds.ds_name ds.ds_type ~mrhb:300.0 ~max:ds.ds_max
                  ~min:ds.ds_min Rrd.VT_Unknown
               )
           else
             None
         )
         dss
      )
  in
  Rrd.rrd_create uid dss rras step timestamp

(* Check if new (enabled) datasources appeared, and add them to the RRD *)
let merge_new_dss rrdi dss =
  let should_enable_ds _ (_, ds) =
    !Rrdd_shared.enable_all_dss || ds.ds_default
  in
  let default_dss = StringMap.filter should_enable_ds dss in
  (* NOTE: It's enough to check if all the default datasources have been added
     to the RRD_INFO, because if a non-default one has been enabled at runtime,
     it's added to the RRD immediately and we don't need to bother *)
  let new_dss =
    StringMap.filter
      (fun ds_name _ -> not (StringMap.mem ds_name rrdi.dss))
      default_dss
  in
  (* fold on Map is not tail-recursive, but the depth of the stack should be
     log of the number of entries at worst, so this should be alright.
     Previous conversions to List are also not tail-recursive with identical
     stack depth *)
  let merge_keys _key a _b = Some a in
  let updated_dss = StringMap.union merge_keys dss rrdi.dss in
  ( updated_dss
  , StringMap.fold
      (fun _key (timestamp, ds) rrd ->
        (* SAFETY: verified that these datasources aren't enabled above
           already, in a more efficient way than RRD does it *)
        rrd_add_ds_unsafe rrd timestamp
          (Rrd.ds_create ds.ds_name ds.Ds.ds_type ~mrhb:300.0 Rrd.VT_Unknown)
      )
      new_dss rrdi.rrd
  )

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

let convert_to_owner_map timestamp dss =
  let consolidate all (owner, ds) =
    let add_ds_to = StringMap.add ds.ds_name (timestamp, ds) in
    let merge = function
      | None ->
          Some (add_ds_to StringMap.empty)
      | Some dss ->
          Some (add_ds_to dss)
    in
    OwnerMap.update owner merge all
  in
  let dss = Seq.fold_left consolidate OwnerMap.empty dss in
  dss

(** Determine datasources missing from this batch, reset them to default
    Unknown values *)
let handle_missing_stats stats paused_vms =
  let named_update = {value= VT_Unknown; transform= Identity} in
  let timestamp = Unix.gettimeofday () in
  let available_stats =
    Seq.flat_map (fun (_, _, dss) -> dss) stats
    |> convert_to_owner_map timestamp
  in
  (* Check which of the enabled data sources are missing from the update batch *)
  let which_missing arr dss =
    Array.fold_left
      (fun missing (ds : Rrd.ds) ->
        if StringMap.mem ds.ds_name dss then
          missing
        else
          StringMap.add ds.ds_name named_update missing
      )
      StringMap.empty arr
  in

  (* NOTE: This only handles missing data sources in an RRD that is partly
     updated, and will not reset the datasources in RRD where all of the
     datasources have gone missing. This is already handled by VM and SR
     shutdown code in XAPI, which will remove such RRDs entirely. *)
  let process_dss ds_owner (dss : (float * Ds.ds) StringMap.t) owner_map =
    match ds_owner with
    | Host -> (
      match !Rrdd_shared.host_rrd with
      | Some host_rrdi ->
          OwnerMap.add ds_owner
            (host_rrdi, which_missing host_rrdi.rrd.rrd_dss dss)
            owner_map
      | None ->
          owner_map
    )
    | VM vm_uuid -> (
      match Hashtbl.find_opt Rrdd_shared.vm_rrds vm_uuid with
      | Some vm_rrdi ->
          if StringSet.mem vm_uuid paused_vms then
            owner_map
          else
            OwnerMap.add ds_owner
              (vm_rrdi, which_missing vm_rrdi.rrd.rrd_dss dss)
              owner_map
      | None ->
          owner_map
    )
    | SR sr_uuid -> (
      match Hashtbl.find_opt Rrdd_shared.sr_rrds sr_uuid with
      | Some sr_rrdi ->
          OwnerMap.add ds_owner
            (sr_rrdi, which_missing sr_rrdi.rrd.rrd_dss dss)
            owner_map
      | None ->
          owner_map
    )
  in

  (* NOTE: We are working on already added and enabled datasources that have
     not been provided a value on this refresh cycle, so nothing needs to be
     added to RRDs *)
  let missing_stats =
    OwnerMap.fold process_dss available_stats OwnerMap.empty
  in
  OwnerMap.iter
    (* NOTE: new_rrd is always false, since it's only 'true' currently if a VM's
       domid does not correspond to rrdi.domid, which would already have been
       fixed by replacing rrdi.domid with the current domid when updating with
       provided datasources *)
      (fun _ds_owner (rrdi, named_updates) ->
      Rrd.ds_update_named rrdi.rrd ~new_rrd:false "rrdd-monitor-reset" timestamp
        named_updates
    )
    missing_stats

(** Updates all of the hosts rrds. We are passed a list of uuids that is used as
    the primary source for which VMs are resident on us. When a new uuid turns
    up that we haven't got an RRD for in our hashtbl, we create a new one. When
    a uuid for which we have an RRD for doesn't appear to have any stats this
    update, we assume that the domain has gone and we stream the RRD to the
    master. We also have a list of the currently rebooting VMs to ensure we
    don't accidentally archive the RRD. *)
let update_rrds uuid_domids paused_vms (uid, timestamp, dss) =
  let uuid_domids = List.to_seq uuid_domids |> StringMap.of_seq in
  let dss = convert_to_owner_map timestamp dss in
  let to_named_updates (_, ds) =
    {value= ds.ds_value; transform= ds.ds_pdp_transform_function}
  in
  let map_keys_to_list dss =
    StringMap.bindings dss |> List.map snd |> List.map snd
  in

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
            let last_updated =
              Rrd.most_recently_last_updated rrdi.rrd.last_updated
            in
            ( last_updated > reading_timestamp
            , abs_float (reading_timestamp -. last_updated)
            )
      in
      if out_of_date then
        error
          "Clock just went backwards by %.0f seconds: RRD data may now be \
           unreliable"
          by_how_much ;
      let process_vm vm_uuid (dss : (float * Ds.ds) Rrd.StringMap.t) =
        match StringMap.find_opt vm_uuid uuid_domids with
        | Some domid -> (
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt vm_rrds vm_uuid with
          | Some rrdi ->
              let updated_dss, rrd = merge_new_dss rrdi dss in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss= updated_dss; domid} ;
              (* CA-34383: Memory updates from paused domains serve no useful
                 purpose. During a migrate such updates can also cause undesirable
                 discontinuities in the observed value of memory_actual. Hence, we
                 ignore changes from paused domains: *)
              if not (StringSet.mem vm_uuid paused_vms) then
                let named_updates = StringMap.map to_named_updates dss in
                Rrd.ds_update_named rrd ~new_rrd:(domid <> rrdi.domid) uid
                  timestamp named_updates
          | None ->
              debug "%s: Creating fresh RRD for VM uuid=%s" __FUNCTION__ vm_uuid ;
              let dss_list = map_keys_to_list dss in
              let rrd = create_fresh_rrd !use_min_max dss_list uid timestamp in
              Hashtbl.replace vm_rrds vm_uuid {rrd; dss; domid}
        )
        | None ->
            info "%s: VM uuid=%s is not resident in this host, ignoring rrds"
              __FUNCTION__ vm_uuid
      in
      let process_sr sr_uuid dss =
        try
          (* First, potentially update the rrd with any new default dss *)
          match Hashtbl.find_opt sr_rrds sr_uuid with
          | Some rrdi ->
              let updated_dss, rrd = merge_new_dss rrdi dss in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss= updated_dss; domid= 0} ;
              let named_updates = StringMap.map to_named_updates dss in
              Rrd.ds_update_named rrd ~new_rrd:false uid timestamp named_updates
          | None ->
              debug "%s: Creating fresh RRD for SR uuid=%s" __FUNCTION__ sr_uuid ;
              let dss_list = map_keys_to_list dss in
              let rrd = create_fresh_rrd !use_min_max dss_list uid timestamp in
              Hashtbl.replace sr_rrds sr_uuid {rrd; dss; domid= 0}
        with _ -> log_backtrace ()
      in
      let process_host dss =
        match !host_rrd with
        | None ->
            debug "%s: Creating fresh RRD for localhost" __FUNCTION__ ;
            let dss_list = map_keys_to_list dss in
            let rrd = create_fresh_rrd true dss_list uid timestamp in
            (* Always always create localhost rrds with min/max enabled *)
            host_rrd := Some {rrd; dss; domid= 0}
        | Some rrdi ->
            let updated_dss, rrd = merge_new_dss rrdi dss in
            host_rrd := Some {rrd; dss= updated_dss; domid= 0} ;
            let named_updates = StringMap.map to_named_updates dss in
            Rrd.ds_update_named rrd ~new_rrd:false uid timestamp named_updates
      in

      let process_dss ds_owner dss =
        match ds_owner with
        | Host ->
            process_host dss
        | VM uuid ->
            process_vm uuid dss
        | SR uuid ->
            process_sr uuid dss
      in
      OwnerMap.iter process_dss dss
  )
