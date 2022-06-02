open Core
open Hex

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

  let take_nibble cpu =
    if String.length cpu.ram >= 4 then
      let nibble = String.sub cpu.ram ~pos:0 ~len:4 in
      print_endline ("Nibble: " ^ nibble);

      { cpu with ram = String.sub cpu.ram ~pos:4 ~len:((String.length cpu.ram) - 4) }
    else
      cpu
end

let c = CPU.new_cpu("./IBMLogo.ch8")
let () =
  print_endline "test";
  print_endline c.ram;
  print_endline (CPU.take_nibble c).ram;
