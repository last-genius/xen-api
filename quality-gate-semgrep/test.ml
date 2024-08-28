let a hosts = List.length hosts = 0

module MainModule =
struct
  let f host hosts =
  (List.mem host hosts && List.length hosts = 1) || List.length hosts = 0
end

let g hosts = List.(length hosts = 0)

let z hosts = for i = 1 to 2 do
  print_endline (string_of_bool (List.(length hosts = 0)))
done

module OtherModule =
struct
  open List
  let v hosts = length hosts = 0
end

module DeeplyNestedModule =
struct
  open List
  let z a = fun hosts -> length hosts = 0
end

module RenamedModule =
struct
  module L = List
  let z a = fun hosts -> L.length hosts = 0
  let z a = fun hosts -> L.(length hosts = 0)
end

module RenamedFalsePositiveModule =
struct
  module X = List
  module L = Hashtbl
  let z a = fun hosts -> L.length hosts = 0
  let z a = fun hosts -> L.(length hosts = 0)
end

module FalsePositiveModule =
struct
  let v hosts = length hosts = 0
end
