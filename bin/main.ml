open Core
open Hex
(* open Sdl *)

module CPU = struct
  let display_width = 64
  let display_height = 48
  let pixel_size = 16
  (* let ramSize = 4096 *)

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

  let new_cpu filename : cpu =
    let file = In_channel.read_all filename in
    {
      ram = show (Hex.of_string file);
      regs = Array.create ~len:16 0;
      dt = 0;
      st = 0;
      i = 0;
      display = Array.make_matrix ~dimx:display_width ~dimy:display_height 0;
      pc = 0;
    }

  let inc cpu = { cpu with pc = cpu.pc + 2 }
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
    let drw cpu vx vy n =
      let x = cpu.regs.(vx) mod 64 and
      y = cpu.regs.(vy) mod 32 and
      sprite = String.sub cpu.ram ~pos:cpu.i ~len:(n * 2) in

      for i = 0 to String.length sprite do
        let row = int_to_byte (int_of_string (String.sub sprite ~pos:(i * 2) ~len:2)) in
          for j = 0 to 8 do
            if row.(j) = 1 then
              begin
                if cpu.display.(x + i).(y + j) = 1 then cpu.regs.(15) <- 1 else ();
                cpu.display.(x + i).(y + j) <- (if cpu.display.(x + i).(y + j) = 0 then 1 else 0);
                ();
              end;
          done;
      done;

      cpu
  end

  let do_op cpu nibble =
    print_endline ("Nibble: " ^ nibble);
    match (String.to_list nibble) with
    | ['0'; '0'; 'e'; '0'] -> OpCode.cls cpu
    | ['1';  n1;  n2; n3;] -> OpCode.jp cpu (hex_to_int [n1; n2; n3])
    | ['6';   x;  k1; k2;] -> OpCode.set cpu (hex_to_int [x]) (hex_to_int [k1; k2;])
    | ['7';   x;  k1; k2;] -> OpCode.add cpu (hex_to_int [x]) (hex_to_int [k1; k2;])
    | ['A';  n1;  n2; n3;] -> OpCode.ld_1 cpu (hex_to_int [n1; n2; n3])
    | ['D';   x;   y;  n;] -> OpCode.drw cpu (hex_to_int [x]) (hex_to_int [y]) (hex_to_int [n])
    | _ -> cpu

  let decode cpu nibble : cpu =
    { (do_op cpu nibble;) with ram = String.sub cpu.ram ~pos:4 ~len:((String.length cpu.ram) - 4); }

  let take_nibble cpu : cpu =
    if String.length cpu.ram >= 4
    then begin
        let nibble = String.sub cpu.ram ~pos:0 ~len:4 in
          decode cpu nibble
      end
    else
      cpu
end

module Graphics = struct
  let create_window width height _title =
    Sdl.init [`VIDEO];
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]

  let render_pixel renderer x y =
    Sdlrender.fill_rect renderer (Sdlrect.make4 ~x ~y ~w:CPU.pixel_size ~h:CPU.pixel_size);
end

let c = CPU.take_nibble (CPU.new_cpu "./IBMLogo.ch8")

let () =
  print_endline c.ram;
  Sdl.init [`VIDEO];
  let window, renderer = Graphics.create_window (CPU.display_width * CPU.pixel_size) (CPU.display_height * CPU.pixel_size) "Title" in
  for i = 1 to 256 do
    Sdlrender.set_draw_color3 renderer ~b:255 ~g:0 ~r:0 ~a:255;
    Graphics.render_pixel renderer 0 0;
    Sdlrender.render_present renderer;
    Sdltimer.delay ~ms:(1000/60);
  done;
  Sdltimer.delay ~ms:2000;
  Sdl.quit ()

