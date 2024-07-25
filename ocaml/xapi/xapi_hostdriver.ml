(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D
module Unixext = Xapi_stdext_unix.Unixext

let invalid_value field value =
  raise Api_errors.(Server_error (invalid_value, [field; value]))

let internal_error fmt =
  Printf.ksprintf
    (fun msg ->
      error "%s" msg ;
      raise Api_errors.(Server_error (internal_error, [msg]))
    )
    fmt

let create ~__context ~host ~name ~versions ~active_version ~selected_version =
  info "%s: %s" __FUNCTION__ name ;
  let ref = Ref.make () in
  let uuid = Uuidx.to_string (Uuidx.make ()) in
  Db.HostDriver.create ~__context ~ref ~uuid ~host ~name ~versions
    ~active_version ~selected_version

let destroy ~__context ~self = Db.HostDriver.destroy ~__context ~self

let discover ~__context =
  let path = "/proc/modules" in
  (* this is fake *)
  try
    let drivers = Unixext.read_lines path in
    let host = Helpers.get_localhost ~__context in
    drivers
    |> List.iter @@ fun line ->
       Scanf.sscanf line "%s %d" @@ fun name _ ->
       create ~__context ~host ~name ~versions:["1"; "2"; "3"]
         ~active_version:"1" ~selected_version:"1"
  with _ -> internal_error "Failed to parse drivers from %s" path

let select ~__context ~self ~version =
  let versions = Db.HostDriver.get_versions ~__context ~self in
  match List.find_opt (String.equal version) versions with
  | Some _ ->
      Db.HostDriver.set_active_version ~__context ~self ~value:version ;
      Db.HostDriver.set_selected_version ~__context ~self ~value:version
  | None ->
      invalid_value "version" version

let deselect ~__context ~self =
  Db.HostDriver.set_active_version ~__context ~self ~value:"" ;
  Db.HostDriver.set_selected_version ~__context ~self ~value:""
