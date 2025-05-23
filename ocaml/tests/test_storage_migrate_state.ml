(*
 * Copyright (C) 2006-2015 Citrix Systems Inc.
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

open Test_highlevel

module StorageMigrateState = struct
  type state_t = unit

  let create_default_state () = Storage_migrate_helper.State.clear ()
end

let sample_send_state =
  Storage_migrate_helper.State.Send_state.
    {
      url= "url"
    ; dest_sr= Storage_interface.Sr.of_string "dest_sr"
    ; remote_info=
        Some
          {
            dp= "remote_dp"
          ; vdi= Storage_interface.Vdi.of_string "mirror_vdi"
          ; url= "remote_url"
          ; verify_dest= true
          }
    ; local_dp= "local_dp"
    ; tapdev=
        Some
          (Tapctl.tapdev_of_rpc
             (Rpc.Dict [("minor", Rpc.Int 0L); ("tapdisk_pid", Rpc.Int 0L)])
          )
    ; failed= false
    ; watchdog= None
    ; live_vm= Storage_interface.Vm.of_string "0"
    ; mirror_key= None
    ; vdi= Storage_interface.Vdi.of_string ""
    }

let sample_receive_state =
  let open Storage_interface in
  Storage_migrate_helper.State.Receive_state.
    {
      sr= Sr.of_string "my_sr"
    ; dummy_vdi= Vdi.of_string "dummy_vdi"
    ; leaf_vdi= Vdi.of_string "leaf_vdi"
    ; leaf_dp= "leaf_dp"
    ; parent_vdi= Vdi.of_string "parent_vdi"
    ; remote_vdi= Vdi.of_string "remote_vdi"
    ; mirror_vm= Vm.of_string "mirror_vm"
    ; url= ""
    ; verify_dest= false
    }

let sample_copy_state =
  Storage_migrate_helper.State.Copy_state.
    {
      base_dp= "base_dp"
    ; leaf_dp= "leaf_dp"
    ; remote_dp= "remote_dp"
    ; dest_sr= Storage_interface.Sr.of_string "dest_sr"
    ; copy_vdi= Storage_interface.Vdi.of_string "copy_vdi"
    ; remote_url= "remote_url"
    ; verify_dest= true
    }

module MapOf = Generic.MakeStateful (struct
  module Io = struct
    open Storage_migrate_helper.State

    type input_t =
      (string * osend operation) option
      * (string * orecv operation) option
      * (string * ocopy operation) option

    type output_t =
      (string * Send_state.t) list
      * (string * Receive_state.t) list
      * (string * Copy_state.t) list

    let string_of_input_t _ = ""

    let string_of_output_t _ = ""
  end

  module State = StorageMigrateState
  open Storage_migrate_helper.State

  let load_input () (send, recv, copy) =
    Option.iter (fun (id, send) -> add id send) send ;
    Option.iter (fun (id, recv) -> add id recv) recv ;
    Option.iter (fun (id, copy) -> add id copy) copy

  let extract_output () _ = map_of ()

  let tests =
    `QuickAndAutoDocumented
      [
        (* Test that operations don't appear from nowhere. *)
        ((None, None, None), ([], [], []))
      ; (* Test that any of the single operations get persisted. *)
        ( (Some ("foo", Send_op sample_send_state), None, None)
        , ([("foo", sample_send_state)], [], [])
        )
      ; ( (None, Some ("bar", Recv_op sample_receive_state), None)
        , ([], [("bar", sample_receive_state)], [])
        )
      ; ( (None, None, Some ("baz", Copy_op sample_copy_state))
        , ([], [], [("baz", sample_copy_state)])
        )
      ]
end)

let test_clear () =
  let open Storage_migrate_helper.State in
  clear () ;
  add "foo" (Send_op sample_send_state) ;
  add "bar" (Recv_op sample_receive_state) ;
  add "baz" (Copy_op sample_copy_state) ;
  clear () ;
  let state = map_of () in
  Alcotest.check
    (Alcotest_comparators.only_compare ())
    "State was not empty after clearing" state ([], [], [])

let test = [("clear", `Quick, test_clear)]

let tests =
  Storage_migrate_helper.State.persist_root := Test_common.working_area ;
  [("storage_migrate_state_map_of", MapOf.tests)]
