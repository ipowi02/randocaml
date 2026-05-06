type coord = {
  x: float; y: float
}
let tuple_as_coord (x, y) = {
  x = x; y = y
}
let coord_as_tuple a = (a.x, a.y)
let user_coords = (47.512635, 19.050191) |> tuple_as_coord
(*TODO!: Unhardcode this later*)
let true_random_bytes n =
  let ic = open_in_bin "/dev/urandom" in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  buf

let true_random_int64 () =
  let bytes = true_random_bytes 8 in
  Bytes.get_int64_be bytes 0 
let true_random_float () = 
  let f = true_random_int64 () |> Int64.to_float in
  Float.abs f /. Int64.to_float Int64.max_int

let random_float_in_range i j =
  let f = true_random_float () in
  i +. f  *. (j -. i)


let gen_rand_point x y r_km = 
  let open Float in 
    let km_per_degree = 111. in

    let theta = random_float_in_range 0. 2. *. pi in
    let d = r_km *. true_random_float () |> sqrt in

    let dx = d *. cos theta in
    let dy = d *. sin theta in

    let cos_lat = x *. pi /. 180. |> cos in
    (x +. dy /. km_per_degree, y +. dx /. km_per_degree *. cos_lat) |> tuple_as_coord


let () = 
  let radius = 10. in
  let rand_user_coord = gen_rand_point user_coords.x user_coords.y radius in
  Printf.printf "(%f, %f)" rand_user_coord.x rand_user_coord.y