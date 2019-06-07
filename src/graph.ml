
let graph_color = [| Graphics.black; Graphics.red; Graphics.blue; Graphics.green; Graphics.magenta; Graphics.cyan |]

let graph_data = Array.init 6 (fun _ -> [])

let add_point g y =
  let a = (Array.get graph_data g) @ [y] in
  Array.set graph_data g a

let draw_data () =
  Array.iteri begin fun i data ->
    Graphics.moveto 0 0;
    Graphics.set_color (Array.get graph_color i);
    List.iteri (fun x y -> Graphics.lineto (x * 4) (y * 4)) data;
  end graph_data




let zoom = 6
let border = 15

let draw_grid x y f =
  let _width = Graphics.size_x () in
  let height = Graphics.size_y () in
  for i = 0 to (x - 1) do
    for j = 0 to (y - 1) do
      let (r, g, b) = f i j in
      let x = border + (i * zoom) in
      let y = (height - border) - (j * zoom) in
      Graphics.set_color (Graphics.rgb r g b);
      Graphics.fill_rect x y zoom zoom
    done
  done

let draw_circle x y r =
  let height = Graphics.size_y () in
  let ax = border + (x * zoom) in
  let ay = (height - border) - (y * zoom) in
  Graphics.set_color Graphics.red;
  Graphics.fill_circle ax ay r

let rec event_loop () =
  let _ = Graphics.wait_next_event [Graphics.Poll] in
  (*Unix.sleep 1;*)
  event_loop ()

let display_graph draw_func =
  Graphics.open_graph "";
  Graphics.moveto 0 0;
  draw_func ();
  try event_loop ()
  with Graphics.Graphic_failure _ -> Printf.printf "Exiting...\n"


