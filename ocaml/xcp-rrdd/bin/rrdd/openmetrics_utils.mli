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

(** A thin layer on top of the Prometheus library to easily add and remove
    new metric sources dynamically, and operate on different types of
    metrics as if they were the same (to store in the same collection, add
    measurements in the same way) *)

module MetricsMap : sig
  type metric_owner = Rrd.ds_owner

  type metric_name = string

  type t

  val create : unit -> t
  (** Registers an internal collection of metrics and a Prometheus collector
      instance. To be called once on startup *)

  val dump_metrics : t -> string

  val add_measurements :
    t -> metric_owner -> (float * Ds.ds) Rrd.StringMap.t -> unit
  (** Adds measurements for a single owner's metrics. Handles registering new
      ones under the hood *)

  val remove_metric : t -> metric_owner -> metric_name -> unit
  (** Removes a single metric *)

  val remove_owner : t -> metric_owner -> unit
  (** Removes all metrics registered to a particular ds_owner *)
end
