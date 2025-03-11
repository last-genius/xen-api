(*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* A single Prometheus metric. Presents the same interface regardless of the
 * type of metric it is. For xcp-rrdd, it doesn't matter if the new measurement
 * is going to replace the old one (gauge) or going to be added to a list
 * (histogram).
 *)

module D = Debug.Make (struct let name = "rrdd_monitor" end)

module Metric = struct
  type t =
    | Gauge of Prometheus.Gauge.t
    | Histogram of Prometheus.DefaultHistogram.t

  type var = VGauge | VHistogram

  let add_measurement t data =
    match t with
    | Gauge x ->
        Prometheus.Gauge.set x data
    | Histogram x ->
        Prometheus.DefaultHistogram.observe x data
end

(* Represents a Prometheus family of metrics - its children (members) have the
 * same name and type but different values for the same labels, e.g.:
 *   metric family xen_vm_cpu_usage (labels = "uuid", "cpu_number")
 *   child 1 - xen_vm_cpu_usage (uuid=X, cpu_number=0)
 *   child 2 - xen_vm_cpu_usage (uuid=X, cpu_number=1)
 *   child 3 - xen_vm_cpu_usage (uuid=Y, cpu_number=0)
 *)
module Family = struct
  type t =
    | GaugeFamily of Prometheus.Gauge.family
    | HistogramFamily of Prometheus.DefaultHistogram.family
end

module MetricsMap = struct
  type metric_owner = Rrd.ds_owner

  type metric_name = string

  type ht = (metric_name, Metric.t) Hashtbl.t

  (* A two-level hash table to be able to easily remove all owner's metrics *)
  type t = (metric_owner, ht) Hashtbl.t * Prometheus.CollectorRegistry.t

  let create () = (Hashtbl.create 37, Prometheus.CollectorRegistry.create ())

  let ds_owner_to_string = function
    | Rrd.VM _ ->
        "vm"
    | Rrd.SR _ ->
        "sr"
    | Rrd.Host ->
        "host"

  type family_info = metric_owner * Metric.var * metric_name

  (* Families should compare and hash the same even if their owners have
     different UUIDs, it's only their type that matters here *)
  module HashtblNode = struct
    type t = family_info

    let equal (owner_x, m_x, name_x) (owner_y, m_y, name_y) =
      match (owner_x, owner_y) with
      | Rrd.VM _, Rrd.VM _ | Rrd.SR _, Rrd.SR _ | Rrd.Host, Rrd.Host ->
          true
      | _ ->
          false && m_x = m_y && name_x = name_y

    let hash (owner, m, name) = Hashtbl.hash (ds_owner_to_string owner, m, name)
  end

  module FamiliesHashtbl = Hashtbl.Make (HashtblNode)

  type families_t = Family.t FamiliesHashtbl.t

  let families : families_t = FamiliesHashtbl.create 37

  let create_family registry (subsystem, typ, name) =
    Printf.printf "create_family for %s\n" name ;
    (* TODO ds_transform; default; min; max etc as labels

       There need to be some custom rules for labels and names. As described
       above, cpu metrics need to have a cpu_number label, for example. Not
       sure how to handle that, I don't want to pollute memory-mapped files
       with more information as they are limited in size. We might need to
       define a table of translation rules from rrdd metrics to prometheus
       metric names and labels ahead of time *)
    let subsystem = ds_owner_to_string subsystem in
    match typ with
    | Metric.VGauge ->
        Family.GaugeFamily
          (Prometheus.Gauge.v_labels ~label_names:["uuid"] ~registry
             ~help:"Metrics for a particular VM" ~namespace:"xen" ~subsystem
             name
          )
    | Metric.VHistogram ->
        Family.HistogramFamily
          (Prometheus.DefaultHistogram.v_labels ~label_names:["uuid"] ~registry
             ~help:"Metrics for a particular VM" ~namespace:"xen" ~subsystem
             name
          )

  let get_or_create_family registry (ds_owner, ds_name) typ =
    let info = (ds_owner, typ, ds_name) in
    let family =
      match FamiliesHashtbl.find_opt families info with
      | Some x ->
          x
      | None ->
          let f = create_family registry info in
          FamiliesHashtbl.replace families info f ;
          f
    in
    family

  let get_owner_uuid ds_owner =
    match ds_owner with Rrd.VM uuid | SR uuid -> uuid | Host -> "host"

  let add_metric (ht, registry) ((metric_owner, metric_name) as metric_info) ds
      =
    let metric_v = match ds.Ds.ds_type with _ -> Metric.VGauge in
    let family = get_or_create_family registry metric_info metric_v in
    let uuid = get_owner_uuid metric_owner in
    let metric =
      match family with
      | Family.GaugeFamily f ->
          Metric.Gauge (Prometheus.Gauge.labels f [uuid])
      | Family.HistogramFamily f ->
          Metric.Histogram (Prometheus.DefaultHistogram.labels f [uuid])
    in
    Hashtbl.replace ht metric_name metric ;
    metric

  let unregister_metric (ht, registry) ds_owner metric_name =
    let metric_info = (ds_owner, metric_name) in
    let uuid = get_owner_uuid ds_owner in
    ( match Hashtbl.find_opt ht metric_name with
    | Some (Metric.Gauge _) -> (
        let family = get_or_create_family registry metric_info Metric.VGauge in
        match family with
        | Family.GaugeFamily f ->
            Prometheus.Gauge.unregister_labels f [uuid]
        | _ ->
            ()
      )
    | Some (Metric.Histogram _) -> (
        let family =
          get_or_create_family registry metric_info Metric.VHistogram
        in
        match family with
        | Family.HistogramFamily f ->
            Prometheus.DefaultHistogram.unregister_labels f [uuid]
        | _ ->
            ()
      )
    | None ->
        ()
    ) ;
    Hashtbl.remove ht metric_name

  (* Legal characters for prometheus metric names are:
      "^[a-zA-Z_:][a-zA-Z0-9_:]*$" (see prometheus.ml:MetricName)
      so we need to, at the very least, change '-' to '_' *)
  let ds_name_to_prometheus_name s =
    Xapi_stdext_std.Xstringext.String.replace "-" "_" s

  let remove_metric (ht, registry) ds_owner ds_name =
    let metric_name = ds_name |> ds_name_to_prometheus_name in
    match Hashtbl.find_opt ht ds_owner with
    | Some x ->
        unregister_metric (x, registry) ds_owner metric_name
    | None ->
        ()

  let remove_owner (ht, registry) metric_owner =
    match Hashtbl.find_opt ht metric_owner with
    | Some x ->
        Hashtbl.iter
          (fun metric_name _ ->
            unregister_metric (x, registry) metric_owner metric_name
          )
          x
    | None ->
        ()

  let add_measurement ((ht : ht), registry) ((_, metric_name) as metric_info) ds
      data =
    let metric =
      match Hashtbl.find_opt ht metric_name with
      | Some metric ->
          metric
      | None ->
          add_metric (ht, registry) metric_info ds
    in
    Metric.add_measurement metric data

  let add_measurements ((ht, registry) : t) metric_owner dss =
    D.warn "add_measurements called for %s" (ds_owner_to_string metric_owner) ;
    let ht =
      match Hashtbl.find_opt ht metric_owner with
      | Some x ->
          x
      | None ->
          let empty = Hashtbl.create 37 in
          Hashtbl.replace ht metric_owner empty ;
          empty
    in
    Rrd.StringMap.iter
      (fun _ (_timestamp, ds) ->
        (* TODO: timestamp. To be looked into. Prometheus has an option to
           "honor provided timestamps", but the OCaml library does not seem to
           have a way to specify these with provided metrics. This will mean
           the same set of issues the timestamp refactoring was supposed to
           avoid will reproduce itself with Prometheus. *)
        let metric_name = ds.Ds.ds_name |> ds_name_to_prometheus_name in
        let info = (metric_owner, metric_name) in
        let value =
          match ds.ds_value with
          | VT_Float x ->
              x
          | VT_Int64 x ->
              Int64.to_float x
          | _ ->
              0.
        in
        add_measurement (ht, registry) info ds value
      )
      dss ;
    D.warn "add_measurements finished for %s" (ds_owner_to_string metric_owner)

  (* TODO: Caching of string so that multiple requests for the same metrics
     don't have to reserialize them *)
  let dump_metrics (_, registry) =
    Prometheus.CollectorRegistry.collect_non_lwt registry
    |> Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output
end
