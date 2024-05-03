open GMain
open Chess_game

(** Initialize GTK library. *)
let _ = GtkMain.Main.init ()

(** Static variables for the GUI Window width and height. *)
let width = 600

let height = 600
let buttons = Array.make_matrix 8 8 (GButton.button ~label:"" ())

type f = {
  mutable row : int;
  mutable col : int;
}

let prev = { row = 4; col = 4 }

(**[gen_pixbuf r c] generates the image to be shown at row [r] and column [c] as
   specified by [Movement.piece_at r c]*)
let gen_pixbuf row col =
  let dim = 65 in
  let img = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
  GdkPixbuf.scale ~dest:img ~width:dim ~height:dim
    (GdkPixbuf.from_file ("assets/" ^ Movement.piece_at row col ^ ".png"));
  img

let set_img location pix =
  ignore (GMisc.image ~pixbuf:pix ~packing:location#set_image ())

let rm_img location = ignore (GMisc.image ~packing:location#set_image ())

(**[castle color king_end rook_start rook_end] updates the gui to perform a
   castle*)
let castle (button : GButton.button) prev_row prev_col row col =
  let perform_castle (button : GButton.button) c ke rs re =
    (* color, king end, rook start, rook end*)
    set_img button (gen_pixbuf prev.row prev.col);
    rm_img buttons.(prev.row).(prev.col);
    set_img buttons.(c).(re) (gen_pixbuf c rs);
    rm_img buttons.(c).(rs);
    Movement.castle_state c ke rs re
  in

  match Movement.type_castle prev_row prev_col row col with
  | "wksc" -> perform_castle button 7 6 7 5
  | "wqsc" -> perform_castle button 7 2 0 3
  | "bksc" -> perform_castle button 0 6 7 5
  | "bqsc" -> perform_castle button 0 2 0 3
  | _ -> failwith "Should never reach this case"

let en_passant_gui (button : GButton.button) prev_row prev_col row col =
  let perform_en_passant (button : GButton.button) pr pc er ec =
    (* pr, pc are the previous row and col; er, ec are the end row and col *)
    let captured_pawn_col = if pc > ec then pc - 1 else pc + 1 in
    let captured_pawn_row = pr in

    (* The pawn remains on the same row *)

    (* Update the GUI: remove the captured pawn image *)
    rm_img buttons.(captured_pawn_row).(captured_pawn_col);

    (* Move the pawn on the board and GUI *)
    set_img button (gen_pixbuf pr pc);
    rm_img buttons.(pr).(pc);

    (* Update internal game state *)
    Movement.update_state pr pc er ec;
    Movement.update_captures er captured_pawn_col
    (* Update captures for en passant *)
  in

  if Movement.is_enpassant prev_row prev_col row col then
    perform_en_passant button prev_row prev_col row col
  else failwith "Invalid en passant attempt"

(**[captured_W_img] stores the images of captured white pieces*)
let captured_W_img = Array.make 15 (GMisc.image ~width:10 ~height:100 ())

(**[captured_W_img] stores the images of captured black pieces*)
let captured_B_img = Array.make 15 (GMisc.image ~width:10 ~height:100 ())

let buffB = GText.buffer ~text:"" ()
let vB = GText.view ~buffer:buffB ~editable:false ~cursor_visible:false ()
let buffW = GText.buffer ~text:"" ()
let vW = GText.view ~buffer:buffW ~editable:false ~cursor_visible:false ()

(**[update_captures captured_images captures pos] updates the gui to reflect the
   captured pieces in [captures]*)
let update_captures_gui () =
  let rec update_cap_gui (captured_img : GMisc.image array) lst n =
    match lst with
    | [] -> ()
    | h :: t ->
        let dim = 30 in
        let img = GdkPixbuf.create ~width:dim ~height:dim ~has_alpha:true () in
        GdkPixbuf.scale ~dest:img ~width:dim ~height:dim
          (GdkPixbuf.from_file ("assets/" ^ h ^ ".png"));
        captured_img.(n)#set_pixbuf img;
        update_cap_gui captured_img t (n + 1)
  in
  update_cap_gui captured_B_img !Movement.captured_B 0;
  update_cap_gui captured_W_img !Movement.captured_W 0;
  let adv = Movement.material_advantage () in
  match fst adv with
  | "same" ->
      buffW#set_text "";
      buffB#set_text ""
  | "B" ->
      buffB#set_text ("+" ^ (adv |> snd |> string_of_int));
      buffW#set_text ""
  | "W" ->
      buffW#set_text ("+" ^ (adv |> snd |> string_of_int));
      buffB#set_text ""
  | _ -> ()

(** [promote_pawn] creates a dialog box that allows the user to choose which
    piece to promote a pawn to. *)
let promote_pawn color =
  let dialog =
    GWindow.dialog ~width:200 ~height:200 ~title:"Promote Pawn" ~modal:true ()
  in
  let vbox = GPack.vbox ~packing:dialog#vbox#add () in
  let pieces = [ "Queen"; "Rook"; "Bishop"; "Knight" ] in
  let result = ref (color ^ "_Queen") in
  (* Default to Queen *)
  List.iter
    (fun p ->
      let button = GButton.button ~label:p ~packing:vbox#add () in
      ignore
        (button#connect#clicked ~callback:(fun () ->
             result := color ^ "_" ^ p;
             dialog#response `DELETE_EVENT)))
    pieces;
  ignore (dialog#run ());
  dialog#destroy ();
  print_endline "Done";
  !result

(* Function to create a square *)
let create_square row col =
  let button = GButton.button ~label:"" () in
  if Movement.has_piece row col then set_img button (gen_pixbuf row col);
  let color = if (row + col) mod 2 = 0 then `NAME "white" else `NAME "green" in
  button#misc#modify_bg [ (`NORMAL, color) ];
  let _ =
    button#connect#clicked ~callback:(fun () ->
        (* if the move from the previous square to the clicked square is
           valid *)
        if Movement.valid_move prev.row prev.col row col then
          let piece = Movement.piece_at prev.row prev.col in

          (**********PROMOTION*********)
          if (piece = "W_Pawn" && row = 0) || (piece = "B_Pawn" && row = 7) then (
            let new_piece = promote_pawn (String.sub piece 0 1) in
            Movement.promote prev.row prev.col new_piece;
            Movement.update_captures row col;
            update_captures_gui ();
            set_img button (gen_pixbuf prev.row prev.col);
            rm_img buttons.(prev.row).(prev.col);
            Movement.update_state prev.row prev.col row col
            (**********PROMOTION********))
          else if Movement.is_valid_castle prev.row prev.col row col then
            (* updates gui and state *)
            castle button prev.row prev.col row col
          else if Movement.is_enpassant prev.row prev.col row col then
            en_passant_gui button prev.row prev.col row col
          else (
            (* regular move or capture *)

            (* update captures table, if necessary*)
            Movement.update_captures row col;
            update_captures_gui ();

            (* Then set new piece at new square and remove old image*)
            set_img button (gen_pixbuf prev.row prev.col);
            rm_img buttons.(prev.row).(prev.col);

            (* update state *)
            Movement.update_state prev.row prev.col row col)
        else (
          (* didn't click on a piece *)
          prev.row <- row;
          prev.col <- col))
  in
  button

(**[create_chessboard_window] creates a window with a standard chess board setup*)
let create_chessboard_window () =
  (* Main window *)
  let window = GWindow.window ~width ~height ~title:"Board" () in
  let _ = window#connect#destroy ~callback:Main.quit in

  (* Vertical box for the Quit button + chessboard *)
  let vbox = GPack.vbox ~width ~height ~packing:window#add () in

  (* Quit button *)
  let quit_button = GButton.button ~label:"Quit" ~packing:vbox#pack () in
  let _ = quit_button#connect#clicked ~callback:Main.quit in

  (* Table for chessbaord *)
  let tableB =
    GPack.table ~rows:1 ~columns:16 ~homogeneous:true ~packing:vbox#add ()
  in
  for cap_p = 0 to 14 do
    let image = GMisc.image ~width:10 ~height:100 () in
    tableB#attach ~left:cap_p ~top:0 ~expand:`BOTH ~fill:`BOTH image#coerce;
    captured_B_img.(cap_p) <- image
  done;
  tableB#attach ~left:15 ~top:0 vB#coerce;

  let table =
    GPack.table ~rows:8 ~columns:8 ~homogeneous:true ~packing:vbox#add ()
  in

  let tableW =
    GPack.table ~rows:1 ~columns:16 ~homogeneous:true ~packing:vbox#add ()
  in
  for cap_p = 0 to 14 do
    let image = GMisc.image ~width:10 ~height:100 () in
    tableW#attach ~left:cap_p ~top:0 ~expand:`BOTH ~fill:`BOTH image#coerce;
    captured_W_img.(cap_p) <- image
  done;
  tableW#attach ~left:15 ~top:0 vW#coerce;

  (* attach buttons to each square in table *)
  for row = 0 to 7 do
    for col = 0 to 7 do
      let square = create_square row col in
      table#attach ~left:col ~top:row ~expand:`BOTH ~fill:`BOTH square#coerce;
      buttons.(row).(col) <- square
    done
  done;

  window#show ();
  ()

(**[create_homescreen_window] creates the inital wondow for game mode selection*)
let create_homescreen_window () =
  let window = GWindow.window ~width ~height ~title:"Home Screen" () in
  let _ = window#connect#destroy ~callback:Main.quit in

  let vbox = GPack.vbox ~packing:window#add () in

  let two_player_button = GButton.button ~packing:vbox#pack () in
  two_player_button#misc#set_size_request ~width:100 ~height:100 ();
  two_player_button#misc#modify_bg [ (`NORMAL, `NAME "gray") ];
  two_player_button#set_border_width 10;
  let _ =
    let lbl = GMisc.label ~packing:two_player_button#set_image () in
    lbl#set_text "Two Player";
    lbl#set_justify `CENTER
  in
  let _ =
    two_player_button#connect#clicked ~callback:(fun () ->
        window#destroy ();
        create_chessboard_window ();
        Main.main ())
  in

  window#show ();
  ()

let main () =
  create_homescreen_window ();
  Main.main ()

let () = main ()
