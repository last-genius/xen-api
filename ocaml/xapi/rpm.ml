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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = "rpm" end)

open D

module Epoch = struct
  type t = int option

  let epoch_none = "(none)"

  exception Invalid_epoch

  let of_string = function
    | s when s = epoch_none || s = "None" ->
        None
    | s -> (
      match int_of_string s with
      | i when i = 0 ->
          None
      | i when i > 0 ->
          Some i
      | _ ->
          raise Invalid_epoch
      | exception _ ->
          raise Invalid_epoch
    )

  let to_string = function Some i -> string_of_int i | None -> epoch_none
end

module Pkg = struct
  type t = {
      name: string
    ; epoch: Epoch.t
    ; version: string
    ; release: string
    ; arch: string
  }

  type order = LT | EQ | GT

  type version_segment = Int of int | Str of string | Tilde

  let string_of_order = function LT -> "<" | EQ -> "=" | GT -> ">"

  let order_of_int = function 0 -> EQ | r when r > 0 -> GT | _ -> LT

  let error_msg = Printf.sprintf "Failed to parse '%s'"

  let parse_epoch_version_release epoch_ver_rel =
    (* The epoch_ver_rel likes, I.E.
     *   "10.1.11-34",
     *   "2:1.1.11-34",
     *   "None:1.1.11-34", or
     *   "(none):1.1.11-34".
     *
     * These may come from:
     *   "yum list updates",
     *   "yum updateinfo list updates", or
     *   "rpm -qa" *)
    let open Rresult.R.Infix in
    ( ( match Astring.String.cuts ~sep:":" epoch_ver_rel with
      | [e; vr] -> (
        try Ok (Epoch.of_string e, vr) with _ -> Error "Invalid epoch"
      )
      | [vr] ->
          Ok (None, vr)
      | _ ->
          Error "Invalid epoch:version-release"
      )
    >>= fun (e, vr) ->
      match Astring.String.cuts ~sep:"-" vr with
      | [v; r] ->
          Ok (e, v, r)
      | _ ->
          Error "Invalid version-release"
    )
    |> function
    | Ok (e, v, r) ->
        (e, v, r)
    | Error _ ->
        let msg = error_msg epoch_ver_rel in
        Helpers.internal_error ~log_err:true "%s" msg

  let of_fullname s =
    (* The s likes, I.E.
     *   "libpath-utils-0.2.1-29.el7.x86_64",
     *   "qemu-dp-2:2.12.0-2.0.11.x86_64", or
     *   "time-(none):1.7-45.el7.x86_64".
     * These may come from "yum updateinfo list updates" and "rpm -qa" *)
    match Astring.String.cut ~rev:true ~sep:"." s with
    | Some (pkg, (("noarch" | "x86_64") as arch)) -> (
      try
        let pos1 = String.rindex pkg '-' in
        let pos2 = String.rindex_from pkg (pos1 - 1) '-' in
        let epoch_ver_rel =
          String.sub pkg (pos2 + 1) (String.length pkg - pos2 - 1)
        in
        let epoch, version, release =
          parse_epoch_version_release epoch_ver_rel
        in
        let name = String.sub pkg 0 pos2 in
        Some {name; epoch; version; release; arch}
      with e ->
        let msg = error_msg s in
        warn "%s: %s" msg (ExnHelper.string_of_exn e) ;
        (* The error should not block update. Ingore it. *)
        None
    )
    | Some _ | None ->
        None

  let to_epoch_ver_rel_json pkg =
    `Assoc
      [
        ("epoch", `String (Epoch.to_string pkg.epoch))
      ; ("version", `String pkg.version)
      ; ("release", `String pkg.release)
      ]

  let to_name_arch_string pkg = pkg.name ^ "." ^ pkg.arch

  let to_fullname pkg =
    match pkg.epoch with
    | Some i ->
        Printf.sprintf "%s-%s:%s-%s.%s.rpm" pkg.name (string_of_int i)
          pkg.version pkg.release pkg.arch
    | None ->
        Printf.sprintf "%s-%s-%s.%s.rpm" pkg.name pkg.version pkg.release
          pkg.arch

  let compare_epoch e1 e2 =
    match (e1, e2) with
    | Some i1, Some i2 ->
        if i1 < i2 then
          LT
        else if i1 = i2 then
          EQ
        else
          GT
    | Some _, None ->
        GT
    | None, Some _ ->
        LT
    | None, None ->
        EQ

  let compare_version_segment s1 s2 =
    match (s1, s2) with
    | Int i1, Int i2 ->
        Int.compare i1 i2 |> order_of_int
    | Str s1, Str s2 ->
        String.compare s1 s2 |> order_of_int
    | Tilde, Tilde ->
        EQ
    | Int _, Str _ ->
        GT
    | Str _, Int _ ->
        LT
    | Tilde, _ ->
        LT
    | _, Tilde ->
        GT

  let split_version_string =
    let r = Re.Posix.compile_pat {|[a-zA-Z]+|[0-9]+|~|} in
    fun s -> s |> Re.all r |> List.map (fun g -> Re.Group.get g 0)

  let normalize v =
    let version_segment_of_string = function
      | "~" ->
          Tilde
      | s -> (
        try Int (int_of_string s) with _ -> Str s
      )
    in
    v |> split_version_string |> List.map version_segment_of_string

  let compare_version_strings s1 s2 =
    (* Compare versions or releases of RPM packages
     * I.E. for "libpath-utils-0.2.1-29.el7.x86_64" and
     * "libpath-utils-0.2.1a-30.el7.x86_64",
     * this function compares:
     * versions between "0.2.1" and "0.2.1a", or
     * releases between "29.el7" and "30.el7".
     * More examples:
     *  "1.2.3" "<" "1.2.4"
     *  "1.2.3" "=" "1.2.3"
     *  "1.2.3" ">" "1.2"
     *  "1.0011" ">" "1.9"
     *  "1.05" "=" "1.5"
     *  "1.0" ">" "1"
     *  "1.0" ">" "1.a"
     *  "2.50" ">" "2.5"
     *  "XS3" "<" "xs2"
     *  "1.2.3" "<" "1.2.3a"
     *  "xs4" "=" "xs.4"
     *  "2a" "<" "2.0"
     *  "2a" "<" "2b"
     *  "1.0" ">" "1.xs2"
     *  "1.0_xs" "=" "1.0.xs"
     *  "1.xs8" ">" "1.xs8~2_1"
     *  "1.2.3" ">" "1.2.3~beta"
     * Some corner cases that don't follow standard RPM versioning conventions
     * with tilde:
     *  "1.2.3~rc1~beta" "<" "1.2.3~rc1"
     *  "1.2.3~" "<" "1.2.3"
     *)
    let rec compare_segments l1 l2 =
      match (l1, l2) with
      | c1 :: t1, c2 :: t2 -> (
        match compare_version_segment c1 c2 with
        | EQ ->
            compare_segments t1 t2
        | r ->
            r
      )
      | Tilde :: _, [] ->
          LT
      | [], Tilde :: _ ->
          GT
      | _ :: _, [] ->
          GT
      | [], _ :: _ ->
          LT
      | [], [] ->
          EQ
    in
    compare_segments (normalize s1) (normalize s2)

  let lt e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | LT, _, _ | EQ, LT, _ | EQ, EQ, LT ->
        true
    | _ ->
        false

  let gt e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | GT, _, _ | EQ, GT, _ | EQ, EQ, GT ->
        true
    | _ ->
        false

  let eq e1 v1 r1 e2 v2 r2 =
    match
      ( compare_epoch e1 e2
      , compare_version_strings v1 v2
      , compare_version_strings r1 r2
      )
    with
    | EQ, EQ, EQ ->
        true
    | _ ->
        false

  let lte e1 v1 r1 e2 v2 r2 = lt e1 v1 r1 e2 v2 r2 || eq e1 v1 r1 e2 v2 r2

  let gte e1 v1 r1 e2 v2 r2 = gt e1 v1 r1 e2 v2 r2 || eq e1 v1 r1 e2 v2 r2
end

let get_latest_version_release vrs =
  List.fold_left
    (fun acc (ver, rel) ->
      match acc with
      | Some (acc_ver, acc_rel) ->
          if Pkg.gt None ver rel None acc_ver acc_rel then
            Some (ver, rel)
          else
            acc
      | None ->
          Some (ver, rel)
    )
    None vrs
