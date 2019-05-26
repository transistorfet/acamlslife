

let _NEXT_ID = ref 1

let get_next_id () =
  let id = !_NEXT_ID in
  _NEXT_ID := !_NEXT_ID + 1;
  id


class terrain size_x size_y = object (_self)
  val width = size_x
  val height = size_y
  val mutable board: int array array = [| |]

end


class creature size = object (_self)
  val id = get_next_id ()
  val mutable x = 0.0
  val mutable y = 0.0
  val mutable size: float = size

  method get_size () =
    size
end


class world size_x size_y = object (_self)
  val terrain = new terrain size_x size_y
  val mutable creatures: creature list = [ ]


  method spawn size =
    let creat = new creature size in
    creatures <- creat :: creatures

  method summary () =
    let creats = List.length creatures in
    let average = (List.fold_left (fun acc creat -> acc +. creat#get_size ()) 0.0 creatures) /. (float_of_int creats) in
    Printf.printf "Creatures: %d\n" creats;
    Printf.printf "Average Size: %f\n" average
end


let run world =
  world#spawn 1.0


let () =
  Printf.printf "A Caml's Life Simulator...\n\n";
  let world = new world 100 100 in
  run world;
  world#summary ()

