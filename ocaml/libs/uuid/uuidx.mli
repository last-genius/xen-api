(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Type-safe UUIDs.
    UUIDs are used in two places:
    + to uniquely name things across the cluster
    + as secure session IDs

    There is the additional constraint that current Xen tools use
    a particular format of UUID (the 16 byte variety generated by fresh ())

    Also, cookies aren't UUIDs and should be put somewhere else.
*)

(** regular UUIDs *)
type without_secret =
  [ `auth
  | `blob
  | `Bond
  | `Certificate
  | `Cluster
  | `Cluster_host
  | `console
  | `crashdump
  | `data_source
  | `Diagnostics
  | `DR_task
  | `event
  | `Feature
  | `generation
  | `Generic
  | `GPU_group
  | `host
  | `host_cpu
  | `host_crashdump
  | `host_metrics
  | `host_patch
  | `LVHD
  | `message
  | `network
  | `network_sriov
  | `Observer
  | `PBD
  | `PCI
  | `PGPU
  | `PIF
  | `PIF_metrics
  | `pool
  | `pool_patch
  | `pool_update
  | `probe_result
  | `PUSB
  | `PVS_cache_storage
  | `PVS_proxy
  | `PVS_server
  | `PVS_site
  | `Repository
  | `role
  | `SDN_controller
  | `secret
  | `SM
  | `SR
  | `sr_stat
  | `subject
  | `task
  | `tunnel
  | `USB_group
  | `user
  | `VBD
  | `VBD_metrics
  | `VDI
  | `vdi_nbd_server_info
  | `VGPU
  | `VGPU_type
  | `VIF
  | `VIF_metrics
  | `VLAN
  | `VM
  | `VM_appliance
  | `VM_group
  | `VM_guest_metrics
  | `VM_metrics
  | `VMPP
  | `VMSS
  | `VTPM
  | `VUSB ]

(** ensures that attempting to unify the type with `session yields
    an error message about a type conflict,
    and also avoids accidentally getting session added to the above
    {!type:without_secret} type.
 *)
type not_secret = [without_secret | `session of [`use_make_uuid_rnd_instead]]

(** session UUIDs and Refs are secret: they are effectively authentication tokens *)
type secret = [`session]

(** all object classes supported by XAPI *)
type all = [without_secret | secret]

(** A 128-bit UUID to identify an object of class 'a. For example the UUID of
    a host has the type ([\[`host\] Uuidx.t]).
    The type parameter is one of {!type:all}
 *)
type 'a t = Uuidm.t constraint 'a = [< all]

val null : [< not_secret] t
(** A null UUID, as defined in RFC 9562 5.9. *)

val make : unit -> [< not_secret] t
(** Create a fresh UUID *)

val make_uuid_urnd : unit -> [< secret] t
(** [make_uuid_urnd ()] generate a UUID using a CSPRNG.
     Currently this reads from /dev/urandom directly. *)

val make_uuid_fast : unit -> [< not_secret] t
(** [make_uuid_fast ()] generate a UUID using a PRNG.
    Don't use this to generate secrets, see {!val:make_uuid_urnd} for that instead.
 *)

val make_v7_uuid_from_parts : int64 -> int64 -> [< not_secret] t
(** For testing only: create a v7 UUID, as defined in RFC 9562 5.7 *)

val make_v7_uuid : unit -> [< not_secret] t
(** Create a fresh v7 UUID, as defined in RFC 9562 5.7. This incorporates a
    POSIX timestamp, such that the alphabetic of any two such UUIDs will match
    the timestamp order - provided that they are at least 245 nanoseconds
    apart. Note that in order to ensure that the timestamps used are
    monotonic, operating time adjustments are ignored and hence timestamps
    only approximate system time. *)

val pp : Format.formatter -> [< not_secret] t -> unit

val equal : 'a t -> 'a t -> bool

val is_uuid : string -> bool

val of_string : string -> [< not_secret] t option
(** Create a UUID from a string. *)

val to_string : 'a t -> string
(** Marshal a UUID to a string. *)

val uuid_of_string : string -> [< not_secret] t option
[@@deprecated "Use of_string"]
(** Deprecated alias for {! Uuidx.of_string} *)

val string_of_uuid : 'a t -> string
[@@deprecated "Use to_string"]
(** Deprecated alias for {! Uuidx.to_string} *)

val of_int_array : int array -> [< not_secret] t option
(** Convert an array to a UUID. *)

val to_int_array : 'a t -> int array
(** Convert a UUID to an array. *)

val uuid_of_int_array : int array -> [< not_secret] t option
[@@deprecated "Use Uuidx.of_int_array"]
(** Deprecated alias for {! Uuidx.of_int_array} *)

val int_array_of_uuid : 'a t -> int array
[@@deprecated "Use Uuidx.to_int_array"]
(** Deprecated alias for {! Uuidx.to_int_array} *)

val of_bytes : string -> [< not_secret] t option

val to_bytes : 'a t -> string

(** A 512-bit cookie. *)
type cookie

val make_cookie : unit -> cookie

val cookie_of_string : string -> cookie

val string_of_cookie : cookie -> string

module Hash : sig
  (** hash a string (deterministically) into a UUID. This uses
      namespace UUID e93e0639-2bdb-4a59-8b46-352b3f408c19. *)

  (* UUID Version 5 derived from argument string and namespace UUID *)
  val string : string -> [< not_secret] t
end

(**/**)

(* just for feature flag, to be removed *)
val make_default : (unit -> [< not_secret] t) ref
