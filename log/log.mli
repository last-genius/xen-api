exception Unknown_level of string
type level = Debug | Info | Warn | Error

type stream_type = Stderr | Stdout | File of string
type stream_log = {
  ty : stream_type;
  channel : out_channel option ref;
  mutex : Mutex.t;
}
type output =
    Stream of stream_log
  | String of string list ref
  | Syslog of string
  | Nil
val int_of_level : level -> int
val string_of_level : level -> string
val level_of_string : string -> level
val mkdir_safe : string -> Unix.file_perm -> unit
val mkdir_rec : string -> Unix.file_perm -> unit
type t = { output : output; mutable level : level; }
val make : output -> level -> t
val opensyslog : string -> level -> t
val openerr : level -> t
val openout : level -> t
val openfile : string -> level -> t
val opennil : unit -> t
val openstring : level -> t
val reopen : t -> t
val close : t -> unit
val string_of_logger : t -> string
val logger_of_string : string -> t
val validate : string -> unit
val set : t -> level -> unit
val gettimestring : unit -> string
val filesize : int ref
val mutex : Mutex.t
val output : t -> ?key:string -> ?extra:string -> level -> string -> unit
val log : t -> level -> ('a, unit, string, unit) format4 -> 'a
val debug : t -> ('a, unit, string, unit) format4 -> 'a
val info : t -> ('a, unit, string, unit) format4 -> 'a
val warn : t -> ('a, unit, string, unit) format4 -> 'a
val error : t -> ('a, unit, string, unit) format4 -> 'a
