let get_device_numbers path =
  let rdev = (Unix.LargeFile.stat path).Unix.LargeFile.st_rdev in
  let major = rdev / 256 and minor = rdev mod 256 in
  (major, minor)

let is_nbd_device path =
  let nbd_device_num = 43 in
  let (major, _) = get_device_numbers path in
  major = nbd_device_num

module Opt = struct
  let default d = function
    | None -> d
    | Some x -> x
end

type t = [
  | `Vhd of string
  | `Raw of string
  | `Nbd of string * string
]

let to_string = function
  | `Vhd x -> "vhd:" ^ x
  | `Raw x -> "raw:" ^ x
  | `Nbd (x,y) -> Printf.sprintf "nbd:(%s,%s)" x y

let of_device path =
  match Tapctl.of_device (Tapctl.create ()) path with
  | _, _, (Some ("vhd", vhd)) -> Some (`Vhd vhd)
  | _, _, (Some ("aio", vhd)) -> Some (`Raw vhd)
  | _, _, _ -> None
  | exception Tapctl.Not_blktap ->
    None
  | exception Tapctl.Not_a_device ->
    None
  | exception _ ->
    None
