module Html = Dom_html
module Tyjs = Tyxml_js.Html5

let d = Html.document
let jstr = Js.string
let fail () = assert false

let string_of_list ls =
  let b = Buffer.create 100 in
  let rec aux = function
      [] -> Buffer.contents b
    | l::ls' -> Buffer.add_char b l; aux ls'
  in aux ls

let list_of_string s = 
  let imax = String.length s in
  let rec loop l = function
      n when n = imax -> List.rev l
    | n -> loop (s.[n] :: l) (n+1) 
  in loop [] 0

let create_canvas w h =
  let d = Html.window##document in
  let c = Html.createCanvas d in
  c##width <- w;
  c##height <- h;
  c

let draw_line ctx (x,y) (x',y') = 
  ctx##beginPath ();
  ctx##moveTo (x,y);
  ctx##lineTo (x',y');
  ctx##stroke ()

let draw_part ctx = function
    1 -> draw_line ctx (0., 200.) (100., 200.)
  | 2 -> draw_line ctx (50., 200.) (50., 0.)
  | 3 -> draw_line ctx (50., 0.) (150., 0.)
  | 4 -> draw_line ctx (50., 50.) (100., 0.)
  | 5 -> draw_line ctx (150., 0.) (150., 20.)
  | 6 -> let pi = 3.14159265 in
    ctx##beginPath ();
    ctx##arc (150., 35., 15., 0., (pi*.2.), Js._true);
    ctx##stroke ()
  | 7 -> draw_line ctx (150., 50.) (150., 100.)
  | 8 -> draw_line ctx (150., 60.) (170., 90.)
  | 9 -> draw_line ctx (150., 60.) (130., 90.)
  | 10 -> draw_line ctx (150., 100.) (170., 140.)
  | 11 -> draw_line ctx (150., 100.) (130., 140.)

let draw n =
  let elt = Js.Opt.get (d##getElementById(jstr "drawing")) fail in
  let cvs = Js.Opt.get (Html.CoerceTo.canvas elt) fail in
  let ctx = cvs##getContext (Html._2d_) in
  ctx##lineWidth <- 3.;
  draw_part ctx n

(* first screen: enter a word *)
let rec get_word _ = 
  let open Tyjs in
  let save_word e =
    if e##keyCode = 13 then begin
      let elt = Js.Opt.get (d##getElementById(jstr "word")) fail in
      let inp = Js.Opt.get (Html.CoerceTo.input elt) fail in
      let word = Js.to_string inp##value in
      let lower_word = String.map Char.lowercase word in
      start lower_word
    end;
    true
  in 
  let word_input = div ~a:[a_id "content"] [
      h1 [pcdata "Hangman"];
      p [pcdata "Enter the word to guess:";
         input ~a:[a_id "word"; a_value ""; 
                   a_input_type `Password; 
                   a_placeholder "Type and press [Enter]";
                   a_onkeypress save_word] ();] 
  ] in
  let node = Tyxml_js.To_dom.of_node word_input in
  let _ = d##body##appendChild(node) in
  Js._false

(* second screen: canvas and play area *)
and start word = 
  let open Tyjs in
  let drawing = div [
      h1 [pcdata "Hangman"];
      canvas ~a:[a_id "drawing"; a_width 200; a_height 200]
        [pcdata "your browser doesn't support canvas"];
    ] in
  let word_alph = div ~a:[a_id "word_alph"] [] in
  let content = Js.Opt.get (d##getElementById(jstr "content")) fail in
  Dom.removeChild d##body content;
  ignore @@ d##body##appendChild(Tyxml_js.To_dom.of_node drawing);
  ignore @@ d##body##appendChild(Tyxml_js.To_dom.of_node word_alph);
  play word 0 []

and play word missed letters = 
  let open Tyjs in
  let letters_left =
    let ls = list_of_string "abcdefghijklmnopqrstuvwxyz" in
    List.filter (fun c -> not @@ List.mem c letters) ls 
  in
  let shown_word = String.map 
      (fun c -> if List.mem c letters then c else '_') word
  in
  let select_letter (e : Dom_html.mouseEvent Js.t) : bool =
    let elt = Dom.eventTarget e in
    let letter = (Js.to_string (Js.Opt.get (elt ## textContent) fail)).[0] in
    begin
      let letters' = letter::letters in
      if List.mem letter (list_of_string word)
      then play word missed letters'
      else (draw (missed + 1); play word (missed + 1) letters')
    end;
    true
  in
  let play_msg = 
    if not @@ String.contains shown_word '_' then 
      div ~a:[a_class ["msg"]] [pcdata "YOU WON !!!"]
    else if missed >= 11 then 
      div ~a:[a_class ["msg"]] [pcdata "YOU LOST !!!"]
    else div [
       p [pcdata "Pick one of the following letters:"];
      div ~a:[a_id "alphabet"] 
        (List.map (fun c -> span ~a:[a_onclick (fun e -> select_letter e)] 
                      [pcdata (String.make 1 c)]) letters_left)
      ]
  in
  let word_alph = div ~a:[a_id "word_alph"] [
      div ~a:[a_id "guessword"] [pcdata shown_word];
       play_msg;
    ] in
  let node = Tyxml_js.To_dom.of_node word_alph in
  let word_alph = Js.Opt.get (d##getElementById(jstr "word_alph")) fail in
  Dom.removeChild d##body word_alph;
  ignore @@ d##body##appendChild(node)

let _ =
  Html.window##onload <- Html.handler get_word
