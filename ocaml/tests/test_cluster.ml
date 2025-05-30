(*
 * Copyright (C) Citrix Systems Inc.
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

open Xapi_cluster

(** NOTE: This mock rpc is also used by tests in test_clustering *)

let test_clusterd_rpc ~__context call =
  let test_token = "test_token" in
  match (call.Rpc.name, call.Rpc.params) with
  | "create", _ ->
      Rpc.
        {success= true; contents= Rpc.String test_token; is_notification= false}
  | ("enable" | "disable" | "destroy" | "leave" | "set-tls-verification"), _ ->
      Rpc.{success= true; contents= Rpc.Null; is_notification= false}
  | "UPDATES.get", _ ->
      Rpc.{success= true; contents= Rpc.Null; is_notification= false}
  | ( ( "Observer.create"
      | "Observer.destroy"
      | "Observer.set_enabled"
      | "Observer.set_attributes"
      | "Observer.set_endpoints"
      | "Observer.init"
      | "Observer.set_trace_log_dir"
      | "Observer.set_export_interval"
      | "Observer.set_host_id"
      | "Observer.set_max_traces"
      | "Observer.set_max_spans"
      | "Observer.set_max_file_size"
      | "Observer.set_compress_tracing_files" )
    , _ ) ->
      Rpc.{success= true; contents= Rpc.Null; is_notification= false}
  | "diagnostics", _ ->
      let open Cluster_interface in
      let id = 1l in
      let me = {addr= IPv4 "192.0.2.1"; id} in
      let cluster_config =
        {
          cluster_name= "xapi-clusterd"
        ; enabled_members= [me]
        ; authkey= "foo"
        ; config_version= 1L
        ; cluster_token_timeout_ms= 20000L
        ; cluster_token_coefficient_ms= 1000L
        ; cluster_stack= Cluster_stack.Corosync3
        }
      in
      let diag =
        {
          config_valid= true
        ; live_cluster_config= Some cluster_config
        ; next_cluster_config= Some cluster_config
        ; saved_cluster_config= Some cluster_config
        ; is_enabled= true
        ; all_members= Some [me]
        ; node_id= None
        ; token= Some test_token
        ; num_times_booted= 1
        ; is_quorate= true
        ; total_votes= 1
        ; expected_votes= 1
        ; quorum= 1
        ; quorum_members= Some [me]
        ; is_running= true
        ; startup_finished= true
        }
      in
      let contents =
        Rpcmarshal.marshal Cluster_interface.diagnostics.Rpc.Types.ty diag
      in
      Rpc.{success= true; contents; is_notification= false}
  | name, params ->
      Alcotest.failf "Unexpected RPC: %s(%s)" name
        (String.concat " " (List.map Rpc.to_string params))

let test_rpc ~__context call =
  match (call.Rpc.name, call.Rpc.params) with
  | "Cluster_host.destroy", [self] ->
      let open API in
      Xapi_cluster_host.destroy ~__context ~self:(ref_Cluster_host_of_rpc self) ;
      Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
  | "Cluster.destroy", [_session; self] ->
      let open API in
      Xapi_cluster.destroy ~__context ~self:(ref_Cluster_of_rpc self) ;
      Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
  | "Cluster_host.get_cluster_config", _ ->
      Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
  | "Cluster.cstack_sync", [_session; self] ->
      let open API in
      Xapi_cluster.cstack_sync ~__context ~self:(ref_Cluster_of_rpc self) ;
      Rpc.{success= true; contents= Rpc.String ""; is_notification= false}
  | name, params ->
      Alcotest.failf "Unexpected RPC: %s(%s)" name
        (String.concat " " (List.map Rpc.to_string params))

let create_cluster ~__context
    ?(cluster_stack = Constants.default_smapiv3_cluster_stack)
    ?(test_clusterd_rpc = test_clusterd_rpc) ?(token_timeout = 1.)
    ?(token_timeout_coefficient = 1.)
    ?(network = Test_common.make_network ~__context ())
    ?(host = Helpers.get_localhost ~__context) () =
  Context.set_test_rpc __context (test_rpc ~__context) ;
  Context.set_test_clusterd_rpc __context (test_clusterd_rpc ~__context) ;
  let pIF = Test_common.make_pif ~__context ~network ~host () in
  Db.PIF.set_IP ~__context ~self:pIF ~value:"192.0.2.1" ;
  Db.PIF.set_currently_attached ~__context ~self:pIF ~value:true ;
  Db.PIF.set_disallow_unplug ~__context ~self:pIF ~value:true ;
  Xapi_cluster.create ~__context ~pIF ~cluster_stack ~pool_auto_join:true
    ~token_timeout ~token_timeout_coefficient

let test_create_destroy_status () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context () in
  pool_destroy ~__context ~self:cluster

let test_enable () =
  let __context = Test_common.make_test_database () in
  let cluster = create_cluster ~__context () in
  (* simulate xapi getting restarted *)
  ( match
      Xapi_clustering.find_cluster_host ~__context
        ~host:Helpers.(get_localhost ~__context)
    with
  | Some self ->
      Xapi_cluster_host.enable ~__context ~self
  | None ->
      Alcotest.fail "Couldn't find freshly-created cluster_host"
  ) ;
  pool_destroy ~__context ~self:cluster

let test_invalid_parameters () =
  let __context = Test_common.make_test_database () in
  let cluster_stack = "invalid_cluster_stack" in
  Alcotest.check_raises
    "Cluster.create should fail upon receiving an invalid cluster stack"
    Api_errors.(Server_error (invalid_cluster_stack, [cluster_stack]))
    (fun () -> create_cluster ~__context ~cluster_stack () |> ignore) ;
  if not (Xapi_fist.allow_corosync2 ()) then (
    Alcotest.check_raises "token_timeout < minimum threshold"
      Api_errors.(Server_error (invalid_value, ["token_timeout"; "0.5"]))
      (fun () -> create_cluster ~__context ~token_timeout:0.5 () |> ignore) ;
    Alcotest.check_raises "token_timeout_coefficient < minimum threshold"
      Api_errors.(
        Server_error (invalid_value, ["token_timeout_coefficient"; "0.6"])
      )
      (fun () ->
        create_cluster ~__context ~token_timeout_coefficient:0.6 () |> ignore
      )
  )

let test_create_cleanup () =
  let __context = Test_common.make_test_database () in
  let test_clusterd_rpc ~__context call =
    match (call.Rpc.name, call.Rpc.params) with
    | "create", _ ->
        Rpc.
          {
            success= false
          ; contents=
              Rpcmarshal.marshal Cluster_interface.error.Rpc.Types.ty
                Cluster_interface.(InternalError "Cluster.create failed")
          ; is_notification= false
          }
    | _, _ ->
        Rpc.{success= true; contents= Rpc.Null; is_notification= false}
  in
  try
    create_cluster ~__context ~test_clusterd_rpc () |> ignore ;
    Alcotest.fail "Cluster.create should have failed"
  with e ->
    print_endline (ExnHelper.string_of_exn e) ;
    Alcotest.(check (slist (Alcotest_comparators.ref ()) compare))
      "Cluster refs should be destroyed" []
      (Db.Cluster.get_all ~__context) ;
    Alcotest.(check (slist (Alcotest_comparators.ref ()) compare))
      "Cluster_host refs should be destroyed" []
      (Db.Cluster_host.get_all ~__context)

let make_cluster_host ~__context ~cluster
    ?(network = Test_common.make_network ~__context ()) () =
  let host = Test_common.make_host ~__context () in
  let pIF = Test_common.make_pif ~__context ~host ~network () in
  Test_common.make_cluster_host ~__context ~cluster ~host ~pIF ()

let test_get_network_succeeds () =
  let __context = Test_common.make_test_database () in
  let network = Test_common.make_network ~__context () in
  let cluster = create_cluster ~__context ~network () in
  Alcotest.check
    Alcotest_comparators.(ref ())
    "One cluster_host" network
    (Xapi_cluster.get_network ~__context ~self:cluster) ;
  (* Test get_network with multiple cluster_hosts on same network *)
  let (_ : API.ref_Cluster_host) =
    make_cluster_host ~__context ~network ~cluster ()
  in
  Alcotest.check
    Alcotest_comparators.(ref ())
    "All cluster_hosts on same network" network
    (Xapi_cluster.get_network ~__context ~self:cluster)

let test_get_network_fails () =
  let __context = Test_common.make_test_database () in
  let network = Test_common.make_network ~__context () in
  let cluster = create_cluster ~__context ~network () in
  let internal_network_error =
    Failure ("No common network found for cluster " ^ Ref.string_of cluster)
  in
  let host = Helpers.get_localhost ~__context in
  ( match Xapi_clustering.find_cluster_host ~__context ~host with
  | Some self ->
      Db.Cluster_host.destroy ~__context ~self
  | None ->
      Alcotest.failf "No cluster_host found on localhost %s" (Ref.string_of host)
  ) ;
  Alcotest.check_raises "No cluster_host exists, only cluster"
    (Failure ("No cluster_hosts found for cluster " ^ Ref.string_of cluster))
    (fun () -> Xapi_cluster.get_network ~__context ~self:cluster |> ignore) ;
  (* Add two cluster_hosts on different networks *)
  for _ = 0 to 1 do
    make_cluster_host ~__context ~cluster () |> ignore
  done ;
  Alcotest.check_raises "Cluster_hosts on different networks"
    internal_network_error (fun () ->
      Xapi_cluster.get_network ~__context ~self:cluster |> ignore
  )

let test =
  [
    ("test_create_destroy_service_status", `Quick, test_create_destroy_status)
  ; ("test_enable", `Quick, test_enable)
  ; ("test_invalid_parameters", `Quick, test_invalid_parameters)
  ; ("test_create_cleanup", `Quick, test_create_cleanup)
  ; ("test_get_network_succeeds", `Quick, test_get_network_succeeds)
  ; ("test_get_network_fails", `Quick, test_get_network_fails)
  ]
