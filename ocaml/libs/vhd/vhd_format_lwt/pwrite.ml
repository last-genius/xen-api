type bigarray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let ( >>= ) = Lwt.( >>= )

(* pwrite with Cstruct instead of bytes *)
external stub_pwrite :
  Unix.file_descr -> bigarray -> file_offset:int -> int -> int -> int
  = "xapi_pwrite_stub"

let pwrite fd buf ~file_offset pos len =
  if pos < 0 || len < 0 || pos > Bigarray.Array1.dim buf - len then
    invalid_arg "Pwrite.pwrite"
  else
    Lwt_unix.blocking fd >>= function
    | true ->
        invalid_arg "blocking"
        (*wait_write ch >>= fun () ->*)
        (*run_job (pwrite_job ch.fd buf ~file_offset pos len)*)
        (*wait_write fd >>= fun () ->*)
        (*run_job (write_bigarray_job (unix_file_descr fd) buf pos len)*)
    | false ->
        Lwt_unix.wrap_syscall Write fd (fun () ->
            stub_pwrite (Lwt_unix.unix_file_descr fd) buf ~file_offset pos len
        )
