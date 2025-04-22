(*
 * Copyright (C) 2025 Vates.
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

module D = Debug.Make (struct let name = "qcow_tool_wrapper" end)

open D

let unimplemented () =
  raise
    (Api_errors.Server_error (Api_errors.unimplemented_in_qcow_tool_wrapper, []))

let run_qcow_tool (progress_cb : int -> unit) (args : string list)
    (ufd : Unix.file_descr) =
  let qcow_tool = !Xapi_globs.qcow_to_stdout in
  info "Executing %s %s" qcow_tool (String.concat " " args) ;
  let open Forkhelpers in
  match
    with_logfile_fd "qcow-tool" (fun log_fd ->
        let pid =
          safe_close_and_exec None (Some ufd) (Some log_fd) [] qcow_tool args
        in
        let _, status = waitpid pid in
        if status <> Unix.WEXITED 0 then (
          error "qcow-tool failed, returning VDI_IO_ERROR" ;
          raise
            (Api_errors.Server_error
               (Api_errors.vdi_io_error, ["Device I/O errors"])
            )
        )
    )
  with
  | Success (out, _) ->
      debug "%s" out
  | Failure (out, e) ->
      error "qcow-tool output: %s" out ;
      raise e

let update_task_progress (__context : Context.t) (x : int) =
  TaskHelper.set_progress ~__context (float_of_int x /. 100.)

let qcow_of_device path =
  let tapdisk_of_path path =
    try
      match Tapctl.of_device (Tapctl.create ()) path with
      | _, str, Some (_, qcow) ->
          debug "Found str %s and file %s" str qcow ;
          Some qcow
      | _ ->
          None
    with Not_found ->
      debug "Device %s has an unknown driver" path ;
      None
  in
  Common_tool_wrapper.find_backend_device path
  |> Option.value ~default:path
  |> tapdisk_of_path

let send (progress_cb : int -> unit) (unix_fd : Unix.file_descr) (path : string)
    (size : Int64.t) =
  debug "Qcow send called with a size of %Ld and path equal to %s" size path ;
  let source =
    match qcow_of_device path with
    | Some p ->
        p
    | _ ->
        failwith "Did not find the source of the VDI"
  in
  let args = [source; path] in
  run_qcow_tool progress_cb args unix_fd
