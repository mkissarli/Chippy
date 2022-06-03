open Core
open Hex
(* open Sdl *)

module CPU = struct
  let display_width = 64
  let display_height = 48
  let pixel_size = 16
  let ram_size = 4096

  type display = int array array

  type cpu = {
    ram: string;
    mutable regs: int array;
    dt: int;
    st: int;
    i: int;
    mutable display: display;
    pc: int;
  }

  let font =
    [|
      0xF0; 0x90; 0x90; 0x90; 0xF0; (* 0 *)
      0x20; 0x60; 0x20; 0x20; 0x70; (* 1 *)
      0xF0; 0x10; 0xF0; 0x80; 0xF0; (* 2 *)
      0xF0; 0x10; 0xF0; 0x10; 0xF0; (* 3 *)
      0x90; 0x90; 0xF0; 0x10; 0x10; (* 4 *)
      0xF0; 0x80; 0xF0; 0x10; 0xF0; (* 5 *)
      0xF0; 0x80; 0xF0; 0x90; 0xF0; (* 6 *)
      0xF0; 0x10; 0x20; 0x40; 0x40; (* 7 *)
      0xF0; 0x90; 0xF0; 0x90; 0xF0; (* 8 *)
      0xF0; 0x90; 0xF0; 0x10; 0xF0; (* 9 *)
      0xF0; 0x90; 0xF0; 0x90; 0x90; (* A *)
      0xE0; 0x90; 0xE0; 0x90; 0xE0; (* B *)
      0xF0; 0x80; 0x80; 0x80; 0xF0; (* C *)
      0xE0; 0x90; 0x90; 0x90; 0xE0; (* D *)
      0xF0; 0x80; 0xF0; 0x80; 0xF0; (* E *)
      0xF0; 0x80; 0xF0; 0x80; 0x80  (* F *)
    |]

  let new_cpu filename : cpu =
    let file = In_channel.read_all filename in
    let data = show (Hex.of_string file) in
    {
      ram = (String.make 512 '0') ^ data ^ (String.make (ram_size - 512 - String.length data) '0');
      regs = Array.create ~len:16 0;
      dt = 0;
      st = 0;
      i = 0;
      display = Array.make_matrix ~dimx:display_width ~dimy:display_height 0;
      pc = 512;
    }

  let inc cpu = { cpu with pc = cpu.pc + 4 }
  let replace l pos v = Array.mapi l ~f:(fun i x -> if i = pos then v else x;)
  let hex_to_int ss = int_of_string ("0x" ^ (String.of_char_list ss))
  let int_to_bArr i =
    let rec int_to_bit acc i =
      if i=0 then acc
      else int_to_bit (i land 1::acc) (i lsr 1)
    in
    let l=int_to_bit [] i in
      Array.of_list  l

  let int_to_byte n =
    let b_arr = int_to_bArr n in
    let pad_number = 8 - (Array.length b_arr) in
    Array.append (Array.create ~len:pad_number 0) b_arr;;


  (* OP CODES *)
  module OpCode = struct
    let cls cpu = { (inc cpu;) with display = Array.make_matrix ~dimx:display_width ~dimy:display_height 0; }
    let jp cpu addr = { cpu with pc = addr }
    let set cpu reg value = { cpu with regs = replace cpu.regs reg value }
    let add cpu reg value = { cpu with regs = replace cpu.regs reg (cpu.regs.(reg) + value) }
    let ld_1 cpu addr = { cpu with i = addr }

    exception MemoryOutOfBounds

    let drw cpu vx vy n =
      if cpu.i > ram_size then raise MemoryOutOfBounds else ();
      cpu.regs.(15) <- 0;
      let x = cpu.regs.(vx) and
      y = cpu.regs.(vy) in

      for i = 1 to n do
        let sprite = String.sub cpu.ram (cpu.i + (cpu.i + i)) 1 in
        for pos = 0 to 7 do
          print_bytes (Hex.to_bytes (Hex.of_string sprite))
        done;
      done;

      cpu

    (* let drw cpu vx vy n = *)
    (*   let x = cpu.regs.(vx) mod 64 and *)
    (*   y = cpu.regs.(vy) mod 32 and *)
    (*   (1* nibble is two hex *1) *)
    (*   sprite = String.sub cpu.ram ~pos:cpu.i ~len:n in *)

    (*   print_endline ("Sprite: " ^ sprite); *)
    (*   for i = 0 to (String.length sprite) - 1 do *)
    (*     let row = int_to_byte (int_of_string (String.sub sprite ~pos:(i) ~len:2)) in *)
    (*     printf ("Bit: "); *)
    (*     for l = 0 to (Array.length row) - 1 do *)
    (*       printf "%d " row.(l) *)
    (*     done; *)
    (*     print_endline ""; *)
    (*       for j = 0 to 7 do *)
    (*         if x + i >= Array.length(cpu.display) || y + j >= Array.length(cpu.display.(0)) then *)
    (*           if row.(j) = 1 then *)
    (*             begin *)
    (*               if cpu.display.(x + i).(y + j) = 1 then cpu.regs.(15) <- 1 else (); *)
    (*               cpu.display.(x + i).(y + j) <- (if cpu.display.(x + i).(y + j) = 0 then 1 else 0); *)
    (*               (); *)
    (*             end; *)
    (*       done; *)
    (*   done; *)

    (*   cpu *)
  end

  let do_op cpu nibble =
    (* print_endline ("Nibble: " ^ nibble); *)
    match (String.to_list nibble) with
    | ['0'; '0'; 'e'; '0'] -> (print_endline "CLS"); OpCode.cls cpu
    | ['1';  n1;  n2; n3;] -> (print_endline ("JP " ^ String.of_char_list [n1; n2; n3;])); OpCode.jp cpu (hex_to_int [n1; n2; n3])
    | ['6';   x;  k1; k2;] -> (print_endline ("SET " ^ String.of_char_list [x; k1; k2])); OpCode.set cpu (hex_to_int [x]) (hex_to_int [k1; k2;])
    | ['7';   x;  k1; k2;] -> (print_endline ("ADD " ^ String.of_char_list [x; k1; k2])); OpCode.add cpu (hex_to_int [x]) (hex_to_int [k1; k2;])
    | ['a';  n1;  n2; n3;] -> (print_endline ("LD1 " ^ String.of_char_list [n1; n2; n3])); OpCode.ld_1 cpu (hex_to_int [n1; n2; n3])
    | ['d';   x;   y;  n;] -> (print_endline ("DRW " ^ String.of_char_list [x; y; n])); OpCode.drw cpu (hex_to_int [x]) (hex_to_int [y]) (hex_to_int [n])
    | _ -> (print_endline ("Unknown command: " ^ nibble)); cpu

  let decode cpu nibble : cpu = do_op cpu nibble

  let take_nibble cpu : cpu =
    if String.length cpu.ram >= 4
    then begin
        let nibble = String.sub cpu.ram ~pos:cpu.pc ~len:4 in
        inc (decode cpu nibble)
      end
    else
      cpu
end

module Graphics = struct
  let create_window width height _title =
    Sdl.init [`VIDEO];
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]

  let render_pixel renderer x y =
    Sdlrender.fill_rect renderer (Sdlrect.make4 ~x ~y ~w:CPU.pixel_size ~h:CPU.pixel_size)

  let clear renderer window width height =
    let
    surf = Sdlwindow.get_surface window and
    color = 0x000000_l and
    rect = Sdlrect.make4 ~x:0 ~y:0 ~w:width ~h:height in
      Sdlsurface.fill_rect surf rect color;
      Sdlwindow.update_surface window
end

  let rec render renderer window width height (c : CPU.cpu) =
  if String.length c.ram > 0 then
    begin
    let cpu = CPU.take_nibble c in
      Graphics.clear renderer window width height;
      for i = 0 to (Array.length cpu.display) - 1 do
        for j = 0 to (Array.length cpu.display.(i))  - 1 do
          if cpu.display.(i).(j) <> 0 then Graphics.render_pixel renderer i j else ();
        done;
      done;
      Sdlrender.render_present renderer;
      (* Sdltimer.delay ~ms:(1000/60); *)
      Sdltimer.delay ~ms:(1000);
      render renderer window width height cpu
    end
  else
    ()

let () =
  let c = CPU.new_cpu "./IBMLogo.ch8" in
  print_endline c.ram;
  Sdl.init [`VIDEO];
  let width = (CPU.display_width * CPU.pixel_size) and
  height = (CPU.display_height * CPU.pixel_size) in
    let window, renderer = Graphics.create_window width height "Title" in
      Sdlrender.set_draw_color3 renderer ~b:255 ~g:0 ~r:0 ~a:255;
      Graphics.clear renderer window width height;
      render renderer window width height c;
      Sdltimer.delay ~ms:2000;
    Sdl.quit ()
