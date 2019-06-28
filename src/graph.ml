

(** Draw Terrain and Creatures **)

let zoom = 6
let border = 15

let draw_grid x y f =
  let height = Graphics.size_y () in
  for i = 0 to (x - 1) do
    for j = 0 to (y - 1) do
      let (r, g, b) = f i j in
      let x = border + (i * zoom) in
      let y = (height - border) - (j * zoom) in
      Graphics.set_color (Graphics.rgb r g b);
      Graphics.fill_rect x y zoom zoom;
      Graphics.set_color Graphics.black;
      Graphics.draw_rect x y zoom zoom
    done
  done

let clear_grid x y =
  let height = Graphics.size_y () in
  let wx = (x * zoom) + (3 * border) in
  let wy = (y * zoom) + (3 * border) in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 (height - wy) wx height

let draw_circle x y r c =
  let height = Graphics.size_y () in
  let ax = border + (int_of_float (x *. float_of_int zoom)) in
  let ay = (height - border) - (int_of_float (y *. float_of_int zoom)) in
  Graphics.set_color c;
  Graphics.fill_circle ax ay r;
  Graphics.set_color Graphics.black;
  Graphics.draw_circle ax ay r

let get_colour i =
  let r = (i mod 8) * 25 + 50 in
  let g = (i / 8 mod 8) * 25 + 50 in
  let b = (i / 64 mod 8) * 25 + 50 in
  Graphics.rgb r g b


(** Draw Side Text **)

let text_data = ref []
let text_area_width = 200
let text_area_height = 400
let line_spacing = 15

let print s =
  text_data := !text_data @ [s]

let reset_text () =
  text_data := []

let flush_text () =
  let width = Graphics.size_x () in
  let height = Graphics.size_y () in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect (width - text_area_width) (height - text_area_height) text_area_width text_area_height;
  List.iteri begin fun i s ->
    Graphics.set_color Graphics.white;
    Graphics.moveto (width - text_area_width) (height - (line_spacing * i) - border);
    Graphics.draw_string s
  end !text_data;
  reset_text ()


(** Draw Graphs **)

let graph_color = [| Graphics.red; Graphics.green; Graphics.blue; Graphics.cyan; Graphics.magenta; Graphics.yellow |]
let graph_data = Array.init 6 (fun _ -> [])

let add_point g y =
  let a = (Array.get graph_data g) @ [y] in
  Array.set graph_data g a

let graph_scale_x = 2
let graph_scale_y = 2

let draw_data () =
  let width = Graphics.size_x () in
  let redraw = ref false in
  Array.iteri begin fun i data ->
    if (List.length data) * graph_scale_x >= (width - text_area_width) then begin
      let rec take l j =
        match (l, j) with
        | ([], _) -> l
        | (_ :: tl, 0) -> tl
        | (_ :: tl, j) -> take tl (j - 1)
      in
      Array.set graph_data i (take data 100);
      redraw := true
    end
  end graph_data;

  if !redraw then begin
    Graphics.set_color Graphics.black;
    Graphics.fill_rect 0 0 width (Graphics.size_y ())
  end;

  Array.iteri begin fun i data ->
    Graphics.moveto 0 0;
    Graphics.set_color (Array.get graph_color i);
    List.iteri (fun x y -> Graphics.lineto (x * graph_scale_x) (y * graph_scale_y)) data;
  end graph_data


(** Initialize and Loop **)

let rec event_loop () =
  let _ = Graphics.wait_next_event [Graphics.Poll] in
  (*Unix.sleep 1;*)
  event_loop ()

let pause () =
  try event_loop ()
  with Graphics.Graphic_failure _ -> Printf.printf "Exiting...\n"

let start () =
  Graphics.open_graph "";
  Graphics.auto_synchronize false;
  let width = Graphics.size_x () in
  let height = Graphics.size_y () in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 width height

let sync () =
  flush_text ();
  Graphics.synchronize ()

let get_key_press () =
  let status = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Poll] in
  if not status.keypressed then
    Char.chr 0
  else begin
    Graphics.wait_next_event [Graphics.Key_pressed] |> ignore;
    status.key
  end

let display_graph draw_func =
  start ();
  draw_func ();
  pause ()


