open Core
open Hex
(* open Sdl *)

module CPU = struct
  let displayWidth = 64
  let displayHeight = 48
  (* let ramSize = 4096 *)

  type display = int array array

  type cpu = {
    mutable ram: string;
    mutable dt: int;
    mutable st: int;
    mutable display: display;
    mutable pc: int;
  }

  let new_cpu filename : cpu =
    let file = In_channel.read_all filename in
    {
      ram = show (Hex.of_string file);
      dt = 0;
      st = 0;
      display = Array.make_matrix ~dimx:displayWidth ~dimy:displayHeight 0;
      pc = 0;
    }

  let do_op nibble =
    print_endline ("Nibble: " ^ nibble);
    match (String.to_list nibble) with
    | ['0'; '0'; 'e'; '0'] -> print_endline "CLS"
    | _ -> print_endline "Error"

  let decode cpu nibble : cpu =
    do_op nibble;
    {
      cpu with
      ram = String.sub cpu.ram ~pos:4 ~len:((String.length cpu.ram) - 4);
      pc = cpu.pc + 2
    }

  let take_nibble cpu : cpu =
    if String.length cpu.ram >= 4
    then begin
        let nibble = String.sub cpu.ram ~pos:0 ~len:4 in
          (* { *)
          (*   decode cpu nibble with *)
          (*     ram = String.sub cpu.ram ~pos:4 ~len:((String.length cpu.ram) - 4); *)
          (*     pc = cpu.pc + 2; *)
          (* } *)
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
