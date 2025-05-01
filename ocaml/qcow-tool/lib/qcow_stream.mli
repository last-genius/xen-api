module Header = Qcow_header
module Cache = Qcow_cache
open Qcow_types

exception Reference_outside_file of int64 * int64

val start_stream_decode :
  Lwt_unix.file_descr -> (int64 * int32 * int64 Cluster.Map.t) Lwt.t
(** Decodes QCOW header and tables from the beginning of the stream,
   constructing a map of data clusters *)

val copy :
     int32
  -> Lwt_unix.file_descr
  -> Lwt_unix.file_descr
  -> int64 Cluster.Map.t
  -> unit Lwt.t
(** Copy data cluster-by-cluster concurrently *)

val stream_decode : Unix.file_descr -> string -> unit
(** Decodes the input QCOW stream, writing out the raw file to the output path
    *)
