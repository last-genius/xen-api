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

type%cstruct t = {offset: uint64_t; len: uint32_t (* data *)} [@@little_endian]

let sizeof = sizeof_t

type t = {
    offset: int64  (** offset on the physical disk *)
  ; len: int32  (** how much data to write *)
}

let end_of_stream = {offset= 0L; len= 0l}

let make ~sector ?(size = 512L) data =
  {offset= Int64.mul sector size; len= Int32.of_int (Cstruct.length data)}

let marshal buf t = set_t_offset buf t.offset ; set_t_len buf t.len

let is_last_chunk buf = get_t_offset buf = 0L && get_t_len buf = 0l

let get_offset = get_t_offset

let get_len = get_t_len
