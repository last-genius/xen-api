(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as prototyped
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
 * GNU Lesser General Public License for more details.
 *)

open Datamodel_types

let check_exn = Xapi_stdext_pervasives.Pervasiveext.check_exn

let current_version =
  Scanf.sscanf Xapi_version.version "%d.%d.%d%[.-]%s"
    (fun mj mn mc _sep _rest -> Printf.sprintf "%d.%d.%d" mj mn mc
  )

(* A version tag starts with a number *)
let is_version name =
  check_exn (fun () -> Scanf.sscanf name "%d" (fun _ -> true))

(* A tag that starts with an older version than the current and ends with "-next" *)
let replace_version version =
  check_exn (fun () ->
      Scanf.sscanf version "%s@-next" (fun v -> v <> current_version)
  )

let update_prototyped {Lifecycle.state; transitions} =
  match (state, Datamodel_common.get_prototyped transitions) with
  | Unreleased_s, _ ->
      (* New element in the datamodel: add version once known *)
      Some (current_version ^ "-next")
  | _, Some v when replace_version v ->
      (* Set version for previously found new element *)
      Some current_version
  | _, Some v when is_version v ->
      (* Preserve established prototype version *)
      Some v
  | _ ->
      (* Published without prototype or named versions: nothing to record *)
      None

let fields fs obj =
  let rec content fs' = function
    | Field fld -> (
      match update_prototyped fld.lifecycle with
      | Some p ->
          let name = Escaping.escape_id fld.full_name in
          (obj.name, name, p) :: fs'
      | None ->
          fs'
    )
    | Namespace (_name, contents) ->
        List.fold_left content fs' contents
  in
  List.fold_left content fs obj.contents

let messages ms obj =
  let content ms' msg =
    match update_prototyped msg.msg_lifecycle with
    | Some p ->
        (obj.name, msg.msg_name, p) :: ms'
    | None ->
        ms'
  in
  List.fold_left content ms obj.messages

let reset = ref false

let _ =
  Arg.parse
    [("-reset", Arg.Set reset, "Output empty functions ")]
    (fun x -> Printf.eprintf "Ignoring argument: %s\n" x)
    "Generate lifecycle replacement module from the datamodel." ;

  let classes, fields, messages =
    if not !reset then
      let api = Datamodel.all_api in
      let process (cs, fs, ms) obj =
        match update_prototyped obj.obj_lifecycle with
        | Some p ->
            ((obj.name, p) :: cs, fields fs obj, messages ms obj)
        | None ->
            (cs, fields fs obj, messages ms obj)
      in
      List.fold_left process ([], [], []) (Dm_api.objects_of_api api)
    else
      ([], [], [])
  in

  print_endline "let prototyped_of_class = function" ;
  List.iter
    (fun (name, p) -> Printf.printf "  | \"%s\" ->\n      Some \"%s\"\n" name p)
    classes ;
  Printf.printf "  | _ ->\n      None\n" ;

  print_endline "\nlet prototyped_of_field = function" ;
  List.iter
    (fun (cls, name, p) ->
      Printf.printf "  | \"%s\", \"%s\" ->\n      Some \"%s\"\n" cls name p
    )
    fields ;
  Printf.printf "  | _ ->\n      None\n" ;

  print_endline "\nlet prototyped_of_message = function" ;
  List.iter
    (fun (cls, name, p) ->
      Printf.printf "  | \"%s\", \"%s\" ->\n      Some \"%s\"\n" cls name p
    )
    messages ;
  Printf.printf "  | _ ->\n      None\n"
