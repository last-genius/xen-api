module Service : sig
  type t = Varstored | Swtpm

  val typ_of : t Rpc.Types.typ

  val to_string : t -> string
end

module Tpm : sig
  (** TPMs have 3 kind of states *)
  type t

  (** key to access a single state *)
  type key

  val key_of_swtpm : string -> key
  (** [key_of_swtpm path] returns a state key represented by [path]. These paths
     are parts of the requests generated by SWTPM and may contain slashes *)

  val deserialize_key : int -> key

  val serialize_key : key -> int
  (** [serialize key] returns the state key represented by [key]. *)

  val empty : t

  val empty_state : string

  val deserialize : string -> t

  val serialize : t -> string

  val update : key -> string -> t -> t

  val lookup : key:key -> t -> string
end