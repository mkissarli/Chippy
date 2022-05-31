open Core

module CPU = struct
  let displayWidth = 64
  let displayHeight = 48
  let ramSize = 4096

  (* type display = int array array *)

  (* type rom = { *)
  (*   buffer: (int array) array; *)
  (*   size: int; *)
  (* } *)

  type cpu = {
    mutable ram: string;
    mutable dt: int;
    mutable st: int;
    mutable display: int;
    mutable pc: int;
    (* rom: rom; *)
  }

  let newCpu filename : cpu =
    let file = In_channel.read_all filename in
    {
      ram = file;
      dt = 0;
      st = 0;
      display = 0;
        (* Array.make_matrix displayWidth, displayHeight, 0 ; *)
      pc = 0;
      (* rom = {buffer = Array.make_matrix 1 1 0; size = 1;}; *)
    }
end

let c = CPU.newCpu("./IBMLogo.ch8")
let () = print_endline "test"; print_endline c.ram;
(* print_endline c.dt; print_endline c.st; print_endline c.display; print_endline c.pc; *)
