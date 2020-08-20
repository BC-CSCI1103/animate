(* filename : animate.ml *)

(***************************************************************)

let speclist = [
  (*  ("-ip", Arg.String set_ip,
   "server's hostname or IP (for communicating clients only)");
  ("-port", Arg.String set_port,
   "server's port number (for communicating clients only)");
  *)]

let rev_argv_list = ref [Sys.argv.(0)]
let anon_fun arg = rev_argv_list := arg :: !rev_argv_list

let usage_msg = "Usage: " ^ Sys.argv.(0) ^
                " [[-ip hostname] -port port-number] args ..."

let () = Arg.parse speclist anon_fun usage_msg
let argv = Array.of_list (List.rev !rev_argv_list)

(* initialTick : 'a -> 'a *)
let initialTick w = w

(* initialMouse : 'a -> 'b -> 'c -> 'd -> 'a *)
let initialMouse w _ _ _ = w

(* initialKey : 'a -> 'b -> 'a *)
let initialKey w _ = w

(* initialStop : 'a -> bool *)
let initialStop _ = false

let start ?(name="Anon")
    ?(width=300)
    ?(height=300)
    ?view
    ?(onTick=initialTick)
    ?(onMouse=initialMouse)
    ?(onKeyPress=initialKey)
    ?(onKeyRelease=initialKey)
    ?(rate=1.0)
    ?(stopWhen=initialStop)
    ?viewLast
    model
  =
  (* initialView : 'a -> Image.t *)
  let initialView _ = Image.empty width height in

  (* view : 'a -> Image.t
     NB: the view in the dispatch is the parameter to start *)
  let view =
    match view with
    | None -> initialView
    | Some view -> view
  in

  (* viewLast : 'a -> Image.t
     NB: the viewLast in the dispatch is the parameter to start *)
  let viewLast =
    match viewLast with
    | None -> initialView
    | Some view -> view in

  (* rate : int
     NB: the rate on the right is from start, measured in ms *)
  let rate = 1000. *. rate in

  let border = 0 in

  let window = GWindow.window ~border_width:border
      ~title:name
      ~show:false
      ~resizable:false
      ~width:width
      ~height:height
      () in

  let modelRef = ref model in

  let clear window =
    try
      window#remove window#child
    with Gpointer.Null -> ()
  in

  let draw_window draw =
    begin
      print_endline "in draw_window ...";
      clear window;
      let drawing_area =
        GMisc.drawing_area ~width ~height ~packing:window#add () in
      let expose drawingarea ev =
        let context = Cairo_gtk.create drawingarea#misc#window in
        begin
          Image.draw context (draw !modelRef);
          true
        end in
      ignore(drawing_area#event#connect#expose (expose drawing_area));
      window#show ()
    end
  in

  let running = ref true in

  (* updateAndView : 'a -> unit *)
  let updateAndView model =
print_endline "in updateAndView ...";
    if !running then
      begin
        modelRef := model;
        if stopWhen model then
          begin
            if !running then running := false else ();
            draw_window viewLast
          end
        else
          draw_window view
      end
    else ()
  in

  (* Mouse *************************************************)
  let mouse_event str ev =
    let nowx = int_of_float(GdkEvent.Button.x ev) - border in
    let nowy = int_of_float(GdkEvent.Button.y ev) - border in
    let model = onMouse !modelRef nowx nowy str
    in
    updateAndView model;
    true
  in

  let mousepressed ev = mouse_event "button_down" ev in
  let mousereleased ev = mouse_event "button_up" ev in

  (* Keys *************************************************)
  let key_event on_key ev =
    let value = GdkEvent.Key.keyval ev in
    let keystr = if value = GdkKeysyms._Up then "up"
      else if value = GdkKeysyms._Down then "down"
      else if value = GdkKeysyms._Left then "left"
      else if value = GdkKeysyms._Right then "right"
      else if value = GdkKeysyms._Return then "\r"
      else if value = GdkKeysyms._Tab then "\t"
      else if value = GdkKeysyms._BackSpace then "\b"
      else GdkEvent.Key.string ev
    in
    let model = on_key !modelRef keystr in
    updateAndView model;
    true
  in

  let keypressed ev  = key_event onKeyPress ev in
  let keyreleased ev = key_event onKeyRelease ev in

  (* The Clock Tick Event *************************************)
  let time_event _ =
    print_string "in time_event ...";
    let model = onTick !modelRef in
    updateAndView model;
    true
  in

  let delete_event _ =
    let _ = if !running then running := false else () in
    let _ = GMain.Main.quit ()
    in
    false
  in

  (* start *)

  window#event#add [`BUTTON_PRESS; `BUTTON_RELEASE; `KEY_PRESS; `KEY_RELEASE];

  print_endline "in top level, about to install callbacks ...";
  (* install the event handler callbacks *)
  ignore (window#event#connect#button_press ~callback:mousepressed);
  ignore (window#event#connect#button_release ~callback:mousereleased);
  ignore (window#event#connect#key_press ~callback:keypressed);
  ignore (window#event#connect#key_release ~callback:keyreleased);
  ignore (GMain.Timeout.add ~ms:(int_of_float rate) ~callback:time_event);
  ignore (window#event#connect#delete ~callback:delete_event);

  (* Get things started, then run again by events *)
  updateAndView (!modelRef);

  GMain.Main.main ()

(* GTK *)
let locale = GtkMain.Main.init ()
