
type state =
| Resting
| Eating
| Walking
| Dead

type world_action =
| Live
| Divide
| Die


let _NEXT_ID = ref 1

let get_next_id () =
  let id = !_NEXT_ID in
  _NEXT_ID := !_NEXT_ID + 1;
  id

let _NEXT_COL = ref 1

let get_next_col () =
  let col = !_NEXT_COL in
  _NEXT_COL := !_NEXT_COL + 1;
  col

let get_current_col () =
  !_NEXT_COL - 1



let pi = 4.0 *. atan 1.0

let normal ?(m=1.0) n =
  let sum = ref 0.0 in
  for _ = 1 to n do
    sum := !sum +. (Random.float m)
  done;
  !sum /. (float_of_int n)

let normal_modifier n factor dev =
  factor +. (factor *. dev *. ((normal n) -. 0.5))

let rand_diff () =
  1.0 -. Random.float 2.0


let wrap_to_float num width =
  if num < 0.0 then
    num +. width
  else
    mod_float num width

let wrap_to num width =
  if num < 0 then
    num + width
  else
    num mod width


module Matrix = struct
  let init m n f =
    let mat = Array.make_matrix m n 0.0 in
    for i = 0 to (m - 1) do
      for j = 0 to (n - 1) do
        Array.set (Array.get mat i) j (f i j)
      done
    done;
    mat

  let rand ?(b=1.0) m n =
    init m n (fun _ _ -> Random.float b)

  let ident m n =
    init m n (fun i j -> if i == j then 1.0 else 0.0)

  let zeros m n =
    init m n (fun _ _ -> 0.0)


  let apply f m =
    let rows = Array.length m in
    let columns = (Array.length (Array.get m 0)) in
    let z = Array.make_matrix rows columns 0.0 in
    for i = 0 to rows - 1 do
      for j = 0 to columns - 1 do
        z.(i).(j) <- f i j m.(i).(j)
      done
    done;
    z

  let add x y =
    let apply_add i j v =
      v +. y.(i).(j)
    in
    apply apply_add x

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
    let rows = Array.length m in
    let columns = (Array.length (Array.get m 0)) in
    for i = 0 to rows - 1 do
      for j = 0 to columns - 1 do
        Printf.printf "%f " m.(i).(j)
      done;
      Printf.printf "\n"
    done;
    Printf.printf "\n"

  let sigmoid m =
    let apply_sigmoid _ _ x =
      1.0 /. (1.0 +. exp (-1.0 *. x))
    in
    apply apply_sigmoid m

end

class stat_collector = object (self)
  val mutable next_value = 0.0
  val mutable value = 0.0
  val mutable diff = 0.0
  val mutable sum = 0.0

  val mutable last_value = 0.0
  val mutable last_diff = 0.0
  val mutable minimum = 0.0
  val mutable maximum = 0.0

  method get_value () = value
  method get_diff () = diff
  method get_sum () = sum
  method get_max () = maximum
  method get_min () = minimum

  method add v =
    next_value <- next_value +. v

  method avg_and_calc d =
    next_value <- next_value /. d;
    self#calculate ()

  method collect v =
    next_value <- v;
    self#calculate ()

  method calculate () =
    last_diff <- diff;
    diff <- next_value -. last_value;
    sum <- sum +. next_value;
    last_value <- value;
    value <- next_value;
    next_value <- 0.0;
    minimum <- min minimum next_value;
    maximum <- max maximum next_value;

end



class tile = object (_self)
  val mutable food = 10.0 *. (Random.float 1.0) ** 10.0
  (*val mutable food = 10.0*)

  val growth_rate = 0.01
  val max_feed_factor = 4.0
  val max_capacity = (Random.float 1.0) ** 3.0 *. 40.0

  initializer
    food <- max_capacity

  method feed amount =
    let actual_amount = min amount (food /. max_feed_factor) in
    food <- food -. actual_amount;
    actual_amount

  method timeslice tick =
    let rate = normal_modifier 2 growth_rate 0.5 in
    let newfood = food +. food *. rate *. (1.0 +. 1.0 *. sin (float_of_int tick)) +. Random.float 0.1 in
    food <- min max_capacity newfood

  method get_food () =
    food

  method get_max_capacity () =
    (*max_capacity*)
    40.0

end


class terrain size_x size_y = object (self)
  val width = size_x
  val height = size_y
  val mutable total_food = 0.0
  val mutable tiles: tile array array = [| |]

  initializer
    tiles <- Array.init width (fun _ -> Array.init height (fun _ -> new tile))

  method get_sizef () =
    (float_of_int width, float_of_int height)

  method get_total_food () =
    total_food

  method get_tile (x, y) =
    Array.get (Array.get tiles x) y

  method timeslice tick =
    total_food <- 0.0;
    for i = 0 to (Array.length tiles) - 1 do
      let subtiles = Array.get tiles i in
      for j = 0 to (Array.length subtiles) - 1 do
        let tile = Array.get subtiles j in
        total_food <- total_food +. tile#get_food ();
        tile#timeslice tick
      done
    done

  method draw () =
    Graph.clear_grid width height;
    let f i j =
      let tile = self#get_tile (i, j) in
      let food = (int_of_float (tile#get_food () /. tile#get_max_capacity () *. 255.0)) in
      (0, 0, food)
    in
    Graph.draw_grid width height f

  method get_inputs (x, y) =
    let l = ref [] in
    for i = x - 1 to x + 1 do
      for j = y - 1 to y + 1 do
        let tile = self#get_tile (wrap_to i width, wrap_to j height) in
        l := tile#get_food () :: !l
      done
    done;
    !l

end


let num_inputs = 12
let num_outputs = 6

let network = [ num_inputs; 20; 40; num_outputs ]

let create_mat ?f:(f=rand_diff) x y =
  Owl.Mat.(empty x y |> map (fun _ -> f ()))

let generate_layer inp out =
  (create_mat out inp, create_mat out 1)

let generate_layers () =
  let layers = ref [] in
  for i = 0 to (List.length network - 2) do
    let inp = List.nth network i in
    let out = List.nth network (i + 1) in
    layers := !layers @ [ generate_layer inp out ]
  done;
  !layers

let sigmoid_mat a =
  Owl.Mat.map (fun x -> 1.0 /. (1.0 +. exp (-1.0 *. x))) a

let sin_mat a =
  Owl.Mat.map (fun x -> sin x) a

class creature_brain = object (self)
  val mutable layers = generate_layers ()

  method infer x =
    (*
    let r1 = Matrix.sigmoid (Matrix.mul input_fc x |> Matrix.add input_b) in
    let r2 = Matrix.sigmoid (Matrix.mul hidden1_fc r1 |> Matrix.add hidden1_b) in
    Matrix.print r2;
    *)

    let (layer1w, layer1b) = List.nth layers 0 in
    let (layer2w, layer2b) = List.nth layers 1 in
    let (layer3w, layer3b) = List.nth layers 2 in

    let r1 = Owl.Mat.(layer1w *@ x + layer1b) |> sin_mat in
    let r2 = Owl.Mat.(layer2w *@ r1 + layer2b) |> sigmoid_mat in
    let r3 = Owl.Mat.(layer3w *@ r2 + layer3b) |> sigmoid_mat in
    (*Owl.Mat.print r3;*)
    r3

  method clone () =
    Oo.copy self

  method alter () =
    let modify x =
      x +. ((Random.float 2.0) -. 1.0) ** 3.0
    in
    layers <- List.map (fun (w, b) -> Owl.Mat.( (map modify w, map modify b) )) layers

  method save dirname =
    for i = 0 to (List.length layers - 1) do
      let (w, b) = List.nth layers i in
      Owl.Mat.save_txt w (dirname ^ Printf.sprintf "/brain%dw.mat" i);
      Owl.Mat.save_txt b (dirname ^ Printf.sprintf "/brain%db.mat" i)
    done

end


class creature ?(brain:creature_brain option) (parent:int) (ancestor:int) (size:float) (energy:float) (variant:int) = object (self)
  val id = get_next_id ()
  val parent = parent
  val ancestor = ancestor

  (* State *)
  val mutable x = 0.0
  val mutable y = 0.0
  val mutable lifespan = 0
  val mutable state = Eating
  val mutable size = size
  val mutable energy = energy
  val mutable direction = 0.0
  val mutable speed = 1.0
  val mutable brain =
    match brain with
    | Some brain -> brain
    | None -> new creature_brain

  (* Parameters *)
  val metabolism_rest = 0.10
  val metabolism_eating = 0.15
  val metabolism_walking = 0.3
  val digest_efficiency = 1.0
  val feed_factor = 0.4
  val growth_factor = 0.1
  val energy_capacity_factor = 10.0
  val size_decrease_factor = 0.5

  (* Stats *)
  val mutable total_eaten = 0.0
  val mutable divisions = 0
  val mutable variant = variant

  method get_id () =
    id

  method get_ancestor () =
    ancestor

  method get_variant () =
    variant

  method set_variant (c:int) =
    variant <- c

  method get_pos () =
    (int_of_float x, int_of_float y)

  method get_posf () =
    (x, y)

  method move nx ny =
    x <- nx;
    y <- ny;

  method get_size () =
    size

  method get_energy () =
    energy

  method max_energy () =
    size *. energy_capacity_factor

  method eat tile =
    let factor = normal_modifier 2 feed_factor 1.0 in
    let max_amount = min (factor *. size) (self#max_energy () -. energy) in
    let actual_amount = tile#feed max_amount in
    total_eaten <- total_eaten +. actual_amount;
    energy <- energy +. (actual_amount *. digest_efficiency);
    self#metabolize metabolism_eating

  method walk (terrain:terrain) =
    let (width, height) = terrain#get_sizef () in
    let nx = wrap_to_float ((speed *. cos direction) +. x) width in
    let ny = wrap_to_float ((speed *. sin direction) +. y) height in
    (*Printf.printf "creature %d walked from (%f, %f) to (%f, %f)\n" id x y nx ny;*)
    x <- nx;
    y <- ny

  method rest () =
    self#metabolize metabolism_rest

  method metabolize metabolism =
    let used = size *. metabolism in
    if used <= energy then
      energy <- energy -. used
    else (
      size <- size -. (used -. energy) *. size_decrease_factor;
      energy <- 0.0
    );

    if energy > self#max_energy () *. 0.5 then begin
      (* when energy is over 50% capacity, you store 5% of that energy as size *)
      let amount = energy *. 0.05 in
      energy <- energy -. amount;
      size <- size +. amount;
      ()
    end

  method get_inputs_vector (terrain:terrain) =
    let m = Owl.Mat.empty num_inputs 1 in
    List.iteri (fun i x -> Owl.Mat.set m i 0 x) (terrain#get_inputs (self#get_pos ()));
    Owl.Mat.set m 9 0 energy;
    Owl.Mat.set m 10 0 size;
    Owl.Mat.set m 11 0 direction;
    m

  method select_action (terrain:terrain) =
    let x = self#get_inputs_vector terrain in
    let r = brain#infer x in

    let (eat, walk, rest, turnl, turnr, sp) = Owl.Mat.( (get r 0 0, get r 1 0, get r 2 0, get r 3 0, get r 4 0, get r 5 0) ) in
    if eat > walk && eat > rest && eat > 0.3 then
      Eating
    else if walk > rest && walk > 0.4 then begin
      (*direction <- direction +. (normal ~m:(pi *. 0.25) 2);*)
      if turnl > turnr && turnl > 0.5 then
        direction <- direction +. 0.1
      else if turnr > 0.5 then
        direction <- direction +. 0.1;
      speed <- sp *. 2.0;
      Walking
    end
    else
      Resting

    (*
    let r = Random.float 1.0 in
    if r < 0.1 then begin
      direction <- direction +. (normal ~m:(pi *. 0.25) 2);
      ()
    end;

    let r = Random.float 1.0 in
    if r < 0.1 then
      Walking
    else if r < 0.8 then
      Eating
    else
      Resting
    *)


  method fitness () =
    if lifespan <= 0 then
      0.0
    else
      total_eaten /. (float_of_int lifespan)
      (* (total_eaten /. (float_of_int lifespan)) *. (float_of_int divisions) *)


  method timeslice (terrain:terrain) (avg_fitness:float) =
    let tile = terrain#get_tile (self#get_pos ()) in
    begin
      lifespan <- lifespan + 1;
      match state with
      | Eating -> self#eat tile
      | Resting -> self#rest ()
      | Walking -> self#walk terrain
      | _ -> self#rest ()
    end;

    state <- self#select_action terrain;

    (*Printf.printf "creature %d is size %f and energy is %f; tile is %f; fitness is %f/%f\n" id size energy (tile#get_food ()) (self#fitness ()) avg_fitness;*)
    if size /. 2.0 > 1.0 && energy /. 2.0 > 1.0 && self#fitness () > avg_fitness then
    (*if size /. 2.0 > 1.0 && energy /. 2.0 > 1.0 && Random.float 1.0 >= 0.95 then*)
      Divide
    else if size < 0.5 then
      self#die ()
    else
      Live

  method divide () =
    divisions <- divisions + 1;
    size <- size /. 2.0;
    energy <- energy /. 2.0;
    total_eaten <- 0.0;
    let newbrain = brain#clone () in
    let newvariant =
      if Random.float 1.0 < 0.2 then begin
        newbrain#alter ();
        get_next_col ()
      end else
        variant
    in
    let creat = new creature ~brain:newbrain id ancestor size energy newvariant in
    (*Printf.printf "creature %d has divided into %d\n" id (creat#get_id ());*)
    creat

  method die () =
    (*Printf.printf "creature %d has died\n" id;*)
    state <- Dead;
    Die

  method save dirname =
    if not (Sys.file_exists dirname) then begin
      Unix.mkdir dirname 0o755
    end;
    let f = open_out (Printf.sprintf "%s/info.txt" dirname) in
    output_string f (Printf.sprintf "id: %d\n" id);
    output_string f (Printf.sprintf "parent: %d\n" parent);
    output_string f (Printf.sprintf "ancestor: %d\n" ancestor);
    output_string f (Printf.sprintf "variant: %d\n" variant);
    output_string f (Printf.sprintf "lifespan: %d\n" lifespan);
    output_string f (Printf.sprintf "size: %f\n" size);
    output_string f (Printf.sprintf "total eaten: %f\n" total_eaten);
    output_string f (Printf.sprintf "divisions: %d\n" divisions);
    close_out f;
    brain#save dirname

end


class world size_x size_y = object (self)
  val terrain = new terrain size_x size_y
  val mutable tick = 0
  val mutable pause = false
  val mutable total_creatures = 0
  val mutable creatures: creature list = [ ]

  val initial_creatures = 1000
  val mutable draw_rate = 10  


  val mutable size_integral = 0.0
  val mutable population = new stat_collector
  val mutable sum_size = new stat_collector
  val mutable avg_energy = new stat_collector
  val mutable avg_fitness = new stat_collector
  val mutable avg_creat_food = new stat_collector
  val mutable sums_variants = Array.make 1 0
  val mutable sums_ancestors = Array.make 1 0

  method creature_count () =
    List.length creatures

  method timeslice tick =
    terrain#timeslice tick;

    creatures <- List.fold_left begin fun acc creat ->
      let action = creat#timeslice terrain (avg_fitness#get_value ()) in
      size_integral <- size_integral +. creat#get_size ();
      match action with
      | Die -> acc
      | Live -> creat :: acc
      | Divide ->
          let newcreat = self#divide creat in
          creat :: newcreat :: acc
    end [] creatures;

    self#collect_statistics ();

    let numcreats = List.length creatures in
    let numcreatsf = float_of_int numcreats in

    let living_variants = Array.fold_left (fun a i -> if i > 0 then a + 1 else a) 0 sums_variants in
    let living_ancestors = Array.fold_left (fun a i -> if i > 0 then a + 1 else a) 0 sums_ancestors in

    Graph.add_point 0 (sum_size#get_value () /. 10.0 |> truncate);
    Graph.add_point 1 (terrain#get_total_food () /. 5000.0 |> truncate);
    Graph.add_point 2 (avg_fitness#get_value () *. 50.0 |> truncate);
    (*Graph.add_point 3 (avg_energy#get_value () |> truncate);*)
    Graph.add_point 3 (living_variants / 4);
    Graph.add_point 4 (truncate numcreatsf / 2);
    Graph.add_point 5 (avg_creat_food#get_value () |> truncate);

    if tick mod draw_rate == 0 then begin
      Graph.print (Printf.sprintf "Tick: %d" tick);
      Graph.print (Printf.sprintf "Total Births: %d" total_creatures);
      Graph.print (Printf.sprintf "Total Deaths: %d" (total_creatures - int_of_float (population#get_value ())));
      Graph.print (Printf.sprintf "Population: %d" (population#get_value () |> int_of_float));
      Graph.print (Printf.sprintf "d/dt Population: %d" (population#get_diff () |> int_of_float));
      Graph.print (Printf.sprintf "d/dt Size: %f" (sum_size#get_diff ()));
      Graph.print (Printf.sprintf "Avg Size: %f" (sum_size#get_value () /. numcreatsf));
      Graph.print (Printf.sprintf "Living Variants: %d/%d" living_variants (get_current_col ()));
      Graph.print (Printf.sprintf "Ancestery: %d" living_ancestors);

      let top10 = ref 0 in
      let variant_pairs = self#sorted_variants () in
      for i = 0 to 9 do
        let (v, n) = variant_pairs.(i) in
        Graph.print ~c:(Graph.get_colour v) (Printf.sprintf "Variant %d:  %d" v n);
        top10 := !top10 + n
      done;
      Graph.print (Printf.sprintf "Top 10 / Total: %f" (float_of_int !top10 /. numcreatsf));
  end

  method sorted_variants () =
    let variant_pairs = Array.mapi (fun i x -> (i, x)) sums_variants in
    Array.sort (fun (_, a) (_, b) -> compare a b * -1) variant_pairs;
    variant_pairs

  method collect_statistics () =
    sums_ancestors <- Array.make (initial_creatures + 1) 0;
    sums_variants <- Array.make (get_current_col () + 1) 0;

    List.iter begin fun creat ->
      creat#get_size () |> sum_size#add;
      creat#get_energy () |> avg_energy#add;
      creat#fitness () |> avg_fitness#add;
      (terrain#get_tile (creat#get_pos ()))#get_food () |> avg_creat_food#add;

      let col = creat#get_variant () in
      sums_variants.(col) <- sums_variants.(col) + 1;
      let anc = creat#get_ancestor () in
      sums_ancestors.(anc) <- sums_ancestors.(anc) + 1
    end creatures;

    let numcreatsf = List.length creatures |> float_of_int in

    sum_size#calculate ();
    avg_energy#avg_and_calc numcreatsf;
    avg_fitness#avg_and_calc numcreatsf;
    avg_creat_food#avg_and_calc numcreatsf;
    population#collect numcreatsf;

  method divide (creat:creature) =
    let newcreat = creat#divide () in
    total_creatures <- total_creatures + 1;

    let (x, y) = creat#get_posf () in
    let distance = creat#get_size () +. newcreat#get_size () in
    let direction = Random.float (pi *. 2.0) in
    let nx = wrap_to_float ((distance *. cos direction) +. x) (float_of_int size_x) in
    let ny = wrap_to_float ((distance *. sin direction) +. y) (float_of_int size_y) in
    newcreat#move nx ny;
    newcreat

  method spawn parent size variant =
    let creat = new creature parent variant size 1.0 variant in
    creatures <- creat :: creatures;
    total_creatures <- total_creatures + 1;
    creat



  method draw () =
    terrain#draw ();
    List.iter begin fun creat ->
      let (x, y) = creat#get_posf () in
      let size = creat#get_size () in
      Graph.draw_circle x y (int_of_float size) (creat#get_variant () |> Graph.get_colour)
    end creatures


  method save_all () =
    let dirname = Printf.sprintf "creatures-%d" (Unix.time () |> int_of_float) in
    if not (Sys.file_exists dirname) then begin
      Unix.mkdir dirname 0o755
    end;
    let f = open_out (Printf.sprintf "%s/stats.txt" dirname) in
    let variant_pairs = self#sorted_variants () in
    for i = 0 to 9 do
      let (v, n) = variant_pairs.(i) in
      output_string f (Printf.sprintf "variant %d: %d\n" v n);
    done;
    close_out f;
    List.iter (fun creat -> creat#save (Printf.sprintf "%s/%d" dirname (creat#get_id ()))) creatures

  method kill_most () =
    let variant_pairs = self#sorted_variants () in
    let top10 = Hashtbl.create 10 in
    for i = 0 to 9 do
      let (v, _) = variant_pairs.(i) in
      Hashtbl.add top10 v true
    done;
    creatures <- List.filter (fun creat -> begin
      match Hashtbl.find_opt top10 (creat#get_variant ()) with
      | None -> false
      | _ -> true
    end) creatures


  method process_key () =
    match Graph.get_key_press () with
    | 'k' -> self#kill_most ()
    | 's' -> self#save_all ()
    | 'p' -> pause <- not pause
    | '+' -> draw_rate <- max (draw_rate / 2) 1
    | '-' -> draw_rate <- min (draw_rate * 2) 20
    | _ -> ()

  method run () =
    for _ = 1 to initial_creatures do
      let creat = self#spawn 0 2.0 (get_next_col ()) in
      let (width, height) = terrain#get_sizef () in
      creat#move (Random.float width) (Random.float height)
    done;

    while self#creature_count () > 0 (*&& tick < 4000*) do
      self#process_key ();

      if not pause then begin
        Graph.reset_text ();
        tick <- tick + 1;
        self#timeslice tick;

        if tick mod draw_rate == 0 then begin
          Graph.draw_data ();
          self#draw ();
          Graph.sync ()
        end;
      end;
    done;
    Printf.printf "Total Creatures: %d\n" total_creatures;
    Printf.printf "Total ticks: %d\n" tick;
    Printf.printf "%!"

end


let () =
  Printf.printf "A Caml's Life Simulator...\n\n";
  Random.self_init ();
  Graph.start ();

  let world = new world 100 100 in
  world#run ();

  let _draw () =
    world#draw ();
    Graph.draw_data ()
  in
  Graph.pause()

