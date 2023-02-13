
let w = 750
let h = 500
let size = 50
let thick = 10
let pos = [|w/3;w/2;2*w/3|]
type dalle = None | Dal of {size:int; under:dalle}

type tower = { 
  qqty : int;
  top : dalle;
  selected : bool;
}

type gamestate = {
  towers : tower list;
  selsize : int;
}

let generate_tower n s=
  (* n quantité s taille du plus gros*)
  let rec gen acc i s=
    if i = 0 then acc
    else let ns = s-thick in gen (Dal {size = ns; under = acc}) (i-1) ns
  in
  gen None n s



let setup () =
  (* obligé par vim ???*)
  Raylib.init_window w h "Vichy";
  Raylib.set_target_fps 60;
  { 
    towers = [
      {qqty = 5; top = generate_tower 5 size; selected = false};
      {qqty = 0; top = generate_tower 0 size; selected = false};
      {qqty = 0; top = generate_tower 0 size; selected = false};
    ];
    selsize = 0}

let rec draw_tower top x y col =
  match top with
  | None -> ()
  | Dal b -> 
    Raylib.draw_rectangle (x-b.size) y (b.size*2) thick col; 
    draw_tower b.under x (y+thick) col

let cerisnt tow =
  (*enlever la cerise du gateau*)
  match tow.top with
  | None -> None
  | Dal d -> d.under

let rec loop gamestate =
  if Raylib.window_should_close () then Raylib.close_window () else

    let open Raylib in
    begin_drawing ();
    clear_background Color.black;

    List.iteri (fun i tow-> 
        draw_rectangle (pos.(i)-10) (h/2-100) 20 100 Color.raywhite;
        draw_tower tow.top pos.(i) (h/2-tow.qqty*thick) 
          (if tow.selected then Color.red else Color.blue);
      ) gamestate.towers;

    end_drawing ();

    let mousex = (int_of_float (Raylib.Vector2.x (Raylib.get_mouse_position ()))) in
    loop { 
      towers = 
        if is_mouse_button_pressed MouseButton.Left then
          List.mapi (fun i tow->
              if (abs (pos.(i) - mousex)) < 20 then 
                if tow.selected then 
                  {qqty = tow.qqty; top = tow.top; selected = false}
                else if gamestate.selsize = 0 then 
                  {qqty = tow.qqty; top = tow.top; selected = true}
                else (*ceriser*)
                  {qqty = tow.qqty + 1; top = Dal {size = gamestate.selsize; under = tow.top}; selected = false}
              else
              if tow.selected then 
                {qqty = tow.qqty - 1; top = cerisnt tow; selected = false}
              else tow) 
            gamestate.towers
        else gamestate.towers ;
      selsize = List.fold_left (+) 0 
          (List.map 
             (fun tow -> 
                if tow.selected then match tow.top with None->0 | Dal d -> d.size
                else 0
             )
             gamestate.towers)}
let marc () = loop (setup ())
