open Core
open Hex
(* open Sdl *)

module CPU = struct
  let displayWidth = 64
  let displayHeight = 48
  (* let ramSize = 4096 *)

  type display = int array array

  type cpu = {
    ram: string;
    regs: int array;
    dt: int;
    st: int;
    i: int;
    display: display;
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
      display = Array.make_matrix ~dimx:displayWidth ~dimy:displayHeight 0;
      pc = 0;
    }

  let inc cpu = { cpu with pc = cpu.pc + 2 }
  let replace l pos v = Array.mapi l ~f:(fun i x -> if i = pos then v else x;)
  let hex_to_int ss = int_of_string ("0x" ^ (String.of_char_list ss))

  (* OP CODES *)
  module OpCode = struct
    let cls cpu = { (inc cpu;) with display = Array.make_matrix ~dimx:displayWidth ~dimy:displayHeight 0; }
    let jp cpu addr = { cpu with pc = addr }
    let set cpu reg value = { cpu with regs = replace cpu.regs reg value }
    let add cpu reg value = { cpu with regs = replace cpu.regs reg (cpu.regs.(reg) + value) }
    let ld_1 cpu addr = { cpu with i = addr }
    let drw cpu _vx _vy _nibble = cpu
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

let c = CPU.new_cpu("./IBMLogo.ch8")
let () =
  print_endline "test";
  print_endline (CPU.take_nibble c).ram;
  Sdl.init [`VIDEO];
  let width, height = (320, 240) in
  let _ =
    Sdlwindow.create2
      ~title:"Let's try SDL2 with OCaml!"
      ~x:`undefined ~y:`undefined ~width ~height
      ~flags:[]
    in
    Sdltimer.delay ~ms:2000;
    Sdl.quit ()
