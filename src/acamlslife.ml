

type state =
| Resting
| Eating
| Walking
| Dead


let _NEXT_ID = ref 1

let get_next_id () =
  let id = !_NEXT_ID in
  _NEXT_ID := !_NEXT_ID + 1;
  id



let normal m n =
  let sum = ref 0.0 in
  for _ = 1 to n do
    sum := !sum +. (Random.float m)
  done;
  !sum /. (float_of_int n)

let normal_modifier n factor dev =
  factor +. (factor *. dev *. ((normal 1.0 n) -. 0.5))

module Matrix = struct
  let init m n f =
    let mat = Array.make_matrix m n 0.0 in
    for i = 0 to (m - 1) do
      for j = 0 to (n - 1) do
        Array.set (Array.get mat i) j (f i j)
      done
    done;
    mat

  let rand m n =
    init m n (fun _ _ -> Random.float 1.0)

  let ident m n =
    init m n (fun i j -> if i == j then 1.0 else 0.0)

  let mul x y =
    let x0 = Array.length x
    and y0 = Array.length y in
    let y1 = if y0 = 0 then 0 else Array.length y.(0) in
    let z = Array.make_matrix x0 y1 0.0 in
    for i = 0 to x0 - 1 do
      for j = 0 to y1 - 1 do
        for k = 0 to y0 - 1 do
          z.(i).(j) <- z.(i).(j) +. x.(i).(k) *. y.(k).(j)
        done
      done
    done;
    z
    
  let print m =
    for i = 0 to (Array.length m) - 1 do
      for j = 0 to (Array.length (Array.get m i)) - 1 do
        Printf.printf "%f " (Array.get (Array.get m i) j)
      done;
      Printf.printf "\n"
    done
end



class tile = object (_self)
  val mutable food = 10.0

  val growth_rate = 0.1
  val max_feed_factor = 2.0
  val max_capacity = 40.0

  method feed amount =
    let actual_amount = min amount (food /. max_feed_factor) in
    food <- food -. actual_amount;
    actual_amount

  method timeslice tick =
    let rate = normal_modifier 2 growth_rate 0.5 in
    let newfood = food +. food *. rate *. (1.0 +. 1.0 *. sin (float_of_int tick)) +. Random.float 0.2 in
    food <- min max_capacity newfood

  method get_food () =
    food

end


class terrain size_x size_y = object (_self)
  val width = size_x
  val height = size_y
  val mutable tiles: tile array array = [| |]

  initializer
    tiles <- Array.init width (fun _ -> Array.init height (fun _ -> new tile))

  method get_tile (x, y) =
    Array.get (Array.get tiles x) y

  method timeslice tick =
    for i = 0 to (Array.length tiles) - 1 do
      let subtiles = Array.get tiles i in
      for j = 0 to (Array.length subtiles) - 1 do
        let tile = Array.get subtiles j in
        tile#timeslice tick
      done
    done
end


class creature (size:float) = object (self)
  val id = get_next_id ()
  val mutable x = 0.0
  val mutable y = 0.0
  val mutable state = Eating
  val mutable size = size
  val mutable energy = 0.0
  val mutable lifespan = 0
  val mutable size_integral = 0.0

  val metabolism_rest = 0.1
  val metabolism_eating = 0.10
  val metabolism_walking = 0.3
  val digest_efficiency = 1.0
  val feed_factor = 0.2
  val growth_factor = 0.2
  val energy_capacity_factor = 2.0

  method get_pos () =
    (int_of_float x, int_of_float y)

  method get_size () =
    size

  method max_energy () =
    size *. energy_capacity_factor

  method eat tile =
    let factor = normal_modifier 2 feed_factor 1.0 in
    let max_amount = min (factor *. size) (self#max_energy () -. energy) in
    let actual_amount = tile#feed max_amount in
    energy <- energy +. (actual_amount *. digest_efficiency);
    self#metabolize metabolism_eating

  method rest () =
    self#metabolize metabolism_rest

  method metabolize metabolism =

    let used = size *. metabolism in
    if used <= energy then
      energy <- energy -. used
    else (
      size <- size -. (used -. energy);
      energy <- 0.0
    );

    if energy > self#max_energy () *. 0.5 then begin
      (* when energy is over 50% capacity, you store 5% of that energy as size *)
      let amount = energy *. 0.05 in
      energy <- energy -. amount;
      size <- size +. amount;
      ()
    end

  method select_action () =
    let r = Random.float 1.0 in
    if energy < 0.1 *. self#max_energy () then
      Eating
    else if r < 0.8 then
      Eating
    else
      Resting


  method timeslice (tile:tile) =
    begin
      lifespan <- lifespan + 1;
      match state with
      | Eating -> self#eat tile
      | Resting -> self#rest ()
      | _ -> self#rest ()
    end;

    state <- self#select_action ();

    size_integral <- size_integral +. size;

    Graph.add_point 0 (truncate size);
    Graph.add_point 1 (truncate (tile#get_food ()));
    Graph.add_point 2 (truncate (size_integral /. (float_of_int lifespan) *. 10.0));
    Graph.add_point 3 (truncate energy);

    Printf.printf "creature %d is size %f and energy is %f; tile is %f\n" id size energy (tile#get_food ());
    if size < 0.5 then
      self#die ()
    else
      true

  method die () =
    Printf.printf "creature %d has died\n" id;
    state <- Dead;
    false

end


class world size_x size_y = object (self)
  val terrain = new terrain size_x size_y
  val mutable tick = 0
  val mutable total_creatures = 0
  val mutable creatures: creature list = [ ]


  val mutable size_integral = 0.0


  method timeslice tick =
    terrain#timeslice tick;
    creatures <- List.filter begin fun creat ->
      let tile = terrain#get_tile (creat#get_pos ()) in
      let has_died = creat#timeslice tile in
      size_integral <- size_integral +. creat#get_size ();
      has_died
    end creatures;

  method spawn size =
    let creat = new creature size in
    creatures <- creat :: creatures;
    total_creatures <- total_creatures + 1

  method creature_count () =
    List.length creatures

  method summary () =
    let creats = List.length creatures in
    (*let average = (List.fold_left (fun acc creat -> acc +. creat#get_size ()) 0.0 creatures) /. (float_of_int creats) in*)
    let average = size_integral /. (float_of_int creats) /. (float_of_int tick) in
    Printf.printf "Total Creatures: %d\n" total_creatures;
    Printf.printf "Average Size: %f\n" average;
    Printf.printf "Total ticks: %d\n" tick

  method run () =
    self#spawn 1.0;

    while self#creature_count () > 0 && tick < 400 do
      tick <- tick + 1;
      self#timeslice tick
    done;

    (*
    let dq1 = ref 0.9 in
    let dq2 = ref 0.3 in
    for _ = 0 to 100 do
      dq1 := 0.3 *. !dq1 +. 0.9 *. !dq2 *. !dq2;
      dq2 := 0.56 *. !dq1 *. !dq1 +. 0.89 *. !dq2;
      Graph.add_point 2 (truncate (!dq1 *. 100.0));
      Graph.add_point 3 (truncate (!dq2 *. 100.0));
    done;
    *)

    (*
    let buckets = Array.make 100 0 in
    for _ = 1 to 1000 do
      let r = normal 1.0 10 in
      let i = truncate (r *. 100.0) in
      Printf.printf "%d %f\n" i r;
      Array.set buckets i ((Array.get buckets i) + 1)
    done;

    for i = 0 to 99 do
      Printf.printf "%d: %d\n" i (Array.get buckets i);
      Graph.add_point 3 (Array.get buckets i)
    done;

    Matrix.print (Matrix.ident 10 1);
    Matrix.print (Matrix.mul (Matrix.ident 10 10) (Matrix.rand 10 10));
    *)

end


let () =
  Printf.printf "A Caml's Life Simulator...\n\n";
  Random.self_init ();
  let world = new world 100 100 in
  world#run ();
  Graph.display_graph Graph.draw_data;
  world#summary ()

