
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


let draw_line x y =
  Graphics.lineto x y

let rec event_loop n =
  let _ = Graphics.wait_next_event [Graphics.Poll] in
  (*Unix.sleep 1;*)
  event_loop n

let display_graph draw_func =
  Graphics.open_graph "";
  Graphics.moveto 0 0;
  draw_func ();
  try event_loop 0
  with Graphics.Graphic_failure _ -> Printf.printf "Exiting...\n"


