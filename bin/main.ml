open Core
open Hex
open Sdlevent
open Sdl

module CPU = struct
  exception File_not_found of string

  let display_width = 64
  let display_height = 32
  let pixel_size = 16
  let ram_size = 4096

  type display = int array array

  type cpu = {
    mutable memory: int array;
    mutable regs: int array;
    mutable dt: int;
    mutable st: int;
    mutable i: int;
    mutable display: display;
    mutable pc: int;
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

  let new_cpu : cpu =
    {
      memory = Array.create ~len:ram_size 0;
      regs = Array.create ~len:16 0;
      dt = 0;
      st = 0;
      i = 0;
      display = Array.make_matrix ~dimx:display_width ~dimy:display_height 0;
      pc = 510; (* 0x200 - 0x2 to account for the fact we increment pc before running ops*)
    }

  (* let get_byte data i = Bytes.get (Bytes.of_string data) i *)
  (* let get_hex data i = Hex.of_char (get_byte data i) *)
  (* let get_hex_byte data i = *)
  (*   let first = (get_hex data ((i - 1) * 2)) and second = (get_hex data ((i - 1) * 2 + 1)) in *)
  (*   [Tuple2.get1 first; Tuple2.get2 first; Tuple2.get1 second; Tuple2.get2 second] *)

  let nth_bit x n = x land (1 lsl n) <> 0
  let nth_bit_value x n = if nth_bit x n then 1 else 0
  let int_to_bArr i =
    let rec int_to_bit acc i =
      if i=0 then acc
      else int_to_bit (i land 1::acc) (i lsr 1)
    in
    let l=int_to_bit [] i in
    let unpadded = Array.of_list  l in
    Array.append (Array.create ~len:(8 - (Array.length unpadded)) 0) unpadded

  let hex_to_decimal ss = int_of_string ("0x" ^ (String.of_char_list ss))
  let decimal_to_hex i = Printf.sprintf "%02X" i

  let hex_to_char_array h =
    let a = Array.create (String.length h) '0' in
    for i = 0 to 3 do
      a.(i) <- Char.of_string (String.sub h i 1);
    done;
    a

  let load_game cpu file =
    print_endline "Hello";
    let in_chan =
    try open_in file
    with Sys_error _ ->
      raise (File_not_found file)
    in

    Array.iteri font ~f:(fun i c -> cpu.memory.(i) <- c);

    let rec load i =
    try
      cpu.memory.(i) <- (int_of_char (input_char in_chan));
      (* print_int cpu.memory.(i); *)
      (* print_char ','; *)
      load (i + 1)
    with End_of_file -> ()
    in
    load 0x200;

    module OpCode = struct
      let cls cpu = cpu.display <- Array.make_matrix ~dimx:display_width ~dimy:display_height 0
      let jp cpu addr = cpu.pc <- addr
      let ld cpu vx byte = cpu.regs.(vx) <- byte
      let add cpu vx byte = cpu.regs.(vx) <- (cpu.regs.(vx) + byte)
      let ld_1 cpu addr = cpu.i <- addr
      let drw cpu vx vy z =
        let
          x = cpu.regs.(vx) % display_width and
          y = cpu.regs.(vy) % display_height
        in
          cpu.regs.(0xF) <- 0;
          for i = 0 to z - 1 do
            let sprite = cpu.memory.(cpu.i + i) in
            if y + i < display_height
            then
              for j = 0 to 7 do
                if x + j < display_width
                then
                  begin
                    if (int_to_bArr sprite).(j) = 1
                    then
                      begin
                      if cpu.display.(x + j).(y + i) = 1 then cpu.regs.(0xF) <- 1 else ();
                      cpu.display.(x + j).(y + i) <- if cpu.display.(x + j).(y + i) = 1 then 0 else 1;
                      end
                    else
                      ();
                  end
                else
                  ()
              done;
              ()
          done

      let se cpu vx byte = if cpu.regs.(vx) = byte then cpu.pc <- cpu.pc + 2 else ()
      let sen cpu vx byte = if cpu.regs.(vx) <> byte then cpu.pc <- cpu.pc + 2 else ()
      let se_2 cpu vx vy = if cpu.regs.(vx) = cpu.regs.(vy) then  cpu.pc <- cpu.pc + 2 else ()
      let ld_dt cpu vx = cpu.dt <- cpu.regs.(vx); print_endline ("****" ^ (decimal_to_hex cpu.dt))
      let ld_vx_vy cpu vx vy = cpu.regs.(vx) <- cpu.regs.(vy)
      let ld_f cpu vx = cpu.i <- cpu.regs.(vx) * 5; print_endline ((Int.to_string cpu.regs.(vx)) ^ ":" ^ (Int.to_string cpu.i))
      let or_vx_vy cpu vx vy = cpu.regs.(vx) <- cpu.regs.(vx) lor cpu.regs.(vy)
      let and_vx_vy cpu vx vy = cpu.regs.(vx) <- cpu.regs.(vx) land cpu.regs.(vy)
      let xor_vx_vy cpu vx vy = cpu.regs.(vx) <- cpu.regs.(vx) lxor cpu.regs.(vy)
      let add_vx_vy cpu vx vy =
        let total = (cpu.regs.(vx) + cpu.regs.(vy)) in
          cpu.regs.(vx) <- total land 0xFF;
          cpu.regs.(0xF) <- if total > 0xFF then 1 else 0
      let sub_vx_vy cpu vx vy =
        let total = cpu.regs.(vx) - cpu.regs.(vy) in
        if total >= 1 then
          begin
            cpu.regs.(0xF) <- 1;
          end
        else
          cpu.regs.(0xF) <- 0;
        cpu.regs.(vx) <- total % 256

      (* These next two vary per implementation... should make this configurable *)
      (* Little endian? *)
      let shr cpu vx =
        cpu.regs.(0xF) <- cpu.regs.(vx) land 0b1;
        cpu.regs.(vx) <- cpu.regs.(vx) lsr 1

      let shl cpu vx =
        cpu.regs.(0xF) <- (cpu.regs.(vx) lsr 7) land 0b1;
        cpu.regs.(vx) <- (cpu.regs.(vx) lsl 1) land 0xFF

      let subn cpu vx vy =
        let total = cpu.regs.(vy) - cpu.regs.(vx) in
        print_endline (Int.to_string(total) ^ ":" ^ Int.to_string(cpu.regs.(vy)) ^ ":" ^ Int.to_string(cpu.regs.(vx)));
        if total >= 1 then
          begin
            cpu.regs.(0xF) <- 1;
          end
        else
          cpu.regs.(0xF) <- 0;
        cpu.regs.(vx) <- total % 256

      let sne cpu vx vy = if cpu.regs.(vx) <> cpu.regs.(vy) then cpu.pc <- cpu.pc + 2 else ()
      let jp_v0 cpu addr = cpu.pc <- addr + cpu.regs.(0x0)
      let rnd cpu vx byte = cpu.regs.(vx) <- (Random.int 256) land byte
      let ld_vx_dt cpu vx = print_endline (decimal_to_hex cpu.dt); cpu.regs.(vx) <- cpu.dt
      let ld_b_vx cpu vx =
        let decimal_string = Printf.sprintf "%03d" cpu.regs.(vx) in
        String.iteri decimal_string (fun i x -> cpu.memory.(cpu.i + i) <- (Int.of_string (String.of_char x)))
      let ld_vx_i cpu vx =
        for i = 0 to vx do
          print_endline ("Vx " ^ (decimal_to_hex i) ^ " = " ^ (decimal_to_hex cpu.memory.(cpu.i + i)));
          cpu.regs.(i) <- cpu.memory.(cpu.i + i)
        done
    end

    let do_op cpu =
      let h1 = decimal_to_hex cpu.memory.(cpu.pc) and h2 = decimal_to_hex cpu.memory.(cpu.pc + 1)
      in
      let nibble = hex_to_char_array (h1 ^ h2)  in
      match nibble with
      | [|'0'; '0'; 'E'; '0'|] ->
          (print_endline "CLS");
          OpCode.cls cpu
      | [|'1';  n1;  n2; n3;|] ->
          (print_endline ("JP " ^ String.of_char_list [n1; n2; n3;]));
          OpCode.jp cpu (hex_to_decimal [n1; n2; n3])
      | [|'3';   x;  k1; k2;|] ->
          (print_endline ("SE " ^ String.of_char_list [x; k1; k2]));
          OpCode.se cpu (hex_to_decimal [x]) (hex_to_decimal [k1; k2;]);
      | [|'4';   x;  k1; k2;|] ->
          (print_endline ("SEN " ^ String.of_char_list [x; k1; k2]));
          OpCode.sen cpu (hex_to_decimal [x]) (hex_to_decimal [k1; k2;])
      | [|'5';   x;  k;  '0';|] ->
          (print_endline ("SE2 " ^ String.of_char_list [x; k;]));
          OpCode.se_2 cpu (hex_to_decimal [x]) (hex_to_decimal [k])
      | [|'6';   x;  k1; k2;|] ->
          (print_endline ("LD " ^ String.of_char_list [x; k1; k2]));
          OpCode.ld cpu (hex_to_decimal [x]) (hex_to_decimal [k1; k2;])
      | [|'7';   x;  k1; k2;|] ->
          (print_endline ("ADD " ^ String.of_char_list [x; k1; k2]));
          OpCode.add cpu (hex_to_decimal [x]) (hex_to_decimal [k1; k2;])
      | [|'8';   x;  y; '0';|] ->
          (print_endline ("LD_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.ld_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '1';|] ->
          (print_endline ("OR_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.or_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '2';|] ->
          (print_endline ("AND_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.and_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '3';|] ->
          (print_endline ("XOR_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.xor_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '4';|] ->
          (print_endline ("ADD_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.add_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '5';|] ->
          (print_endline ("SUB_VX_VY " ^ String.of_char_list [x; y]));
          OpCode.sub_vx_vy cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; '6';|] ->
          (print_endline ("SHR " ^ String.of_char_list [x]));
          OpCode.shr cpu (hex_to_decimal [x])
      | [|'8';   x;  y; '7';|] ->
          (print_endline ("SUBN " ^ String.of_char_list [x; y]));
          OpCode.subn cpu (hex_to_decimal [x]) (hex_to_decimal [y;])
      | [|'8';   x;  y; 'E';|] ->
          (print_endline ("SHL " ^ String.of_char_list [x]));
          OpCode.shl cpu (hex_to_decimal [x])
      | [|'9';   x;  y; '0';|] ->
          (print_endline ("SNE " ^ String.of_char_list [x; y]));
          OpCode.sne cpu (hex_to_decimal [x]) (hex_to_decimal [y])
      | [|'A';  n1;  n2; n3;|] ->
          (print_endline ("LD1 " ^ String.of_char_list [n1; n2; n3]));
          OpCode.ld_1 cpu (hex_to_decimal [n1; n2; n3])
      | [|'B';  n1;  n2; n3;|] ->
          (print_endline ("JP_V0 " ^ String.of_char_list [n1; n2; n3;]));
          OpCode.jp_v0 cpu (hex_to_decimal [n1; n2; n3])
      | [|'C';   x;  k1; k2;|] ->
          (print_endline ("RND " ^ String.of_char_list [x; k1; k2]));
          OpCode.rnd cpu (hex_to_decimal [x]) (hex_to_decimal [k1; k2;])
      | [|'D';   x;   y;  n;|] ->
          (print_endline ("DRW " ^ String.of_char_list [x; y; n]));
          OpCode.drw cpu (hex_to_decimal [x]) (hex_to_decimal [y]) (hex_to_decimal [n])
      | [|'F';  vx; '0'; '7';|] ->
          (print_endline ("LD_VX_DT " ^ String.of_char_list [vx]));
          OpCode.ld_vx_dt cpu (hex_to_decimal [vx])
      | [|'F';  vx; '1'; '5';|] ->
          (print_endline ("LD DT " ^ String.of_char_list [vx]));
          OpCode.ld_dt cpu (hex_to_decimal [vx])
      | [|'F';  vx; '2'; '9';|] ->
          (print_endline ("LD F " ^ String.of_char_list [vx]));
          OpCode.ld_f cpu (hex_to_decimal [vx])
      | [|'F';  vx; '3'; '3';|] ->
          (print_endline ("LD_B_VX " ^ String.of_char_list [vx]));
          OpCode.ld_b_vx cpu (hex_to_decimal [vx])
      | [|'F';  vx; '6'; '5';|] ->
          (print_endline ("LD_VX_I " ^ String.of_char_list [vx]));
          OpCode.ld_vx_i cpu (hex_to_decimal [vx])
      | _ -> (print_endline ("Unknown command: " ^ h1 ^ h2))

    let modulus_registers cpu =
      for i = 0 to 15 do
        cpu.regs.(i) <- cpu.regs.(i) mod 256
      done

    let next cpu =
      cpu.pc <- cpu.pc + 2;
      do_op cpu;
      modulus_registers cpu;
end

module Graphics = struct
  let create_window_and_renderer width height =
    let title = "CHIP-8" in
      Sdlwindow.create2 ~title
        ~x:`undefined ~y:`undefined ~width ~height
        ~flags:[Sdlwindow.Resizable]

  let clear window width height =
    let
      surf = Sdlwindow.get_surface window and
      color = 0x000000_l and
      rect = Sdlrect.make4 0 0 width height
    in
      Sdlsurface.fill_rect surf rect color;
      Sdlwindow.update_surface window

  let render window display =
    let
      surf = Sdlwindow.get_surface window and
      color = 0xFFFFFF_l
    in
    for i = 0 to CPU.display_width - 1 do
      for j = 0 to CPU.display_height - 1 do
        if display.(i).(j) <> 0
        then
          let
            rect = Sdlrect.make4 (i * CPU.pixel_size) (j * CPU.pixel_size) (CPU.pixel_size - 4) (CPU.pixel_size - 4)
          in
            Sdlsurface.fill_rect surf rect color
        else
          ()
      done;
    done;
    Sdlwindow.update_surface window
end

module Inputs = struct
  let proc_events = function
    | Sdlevent.KeyDown { scancode = Sdlscancode.ESCAPE } ->
      print_endline "Goodbye";
      exit 0
    | e -> ()
      (* print_endline (Sdlevent.to_string e) *)
end

let c = CPU.new_cpu
let time_in_int = fun _ -> Time_ns.to_int_ns_since_epoch (Time_ns.now ())
type timer = { mutable delay: int; mutable dt: int; }
let timers = { delay = 0; dt = 0 }
let last_time = { delay = time_in_int(); dt = time_in_int() }
let hz_to_nanosecs x = x * 1000000000
let seconds_to_nanoseconds x = x * 1_000_000_000

let () =
  Sdl.init [`VIDEO];
  CPU.load_game c "./chip8-test-rom.ch8";
  (* CPU.load_game c "./IBMLogo.ch8"; *)
  let width, height = (CPU.display_width * CPU.pixel_size, CPU.display_height * CPU.pixel_size) in
  let window = Graphics.create_window_and_renderer width height
  in
  let rec event_loop () =
    match Sdlevent.poll_event () with
    | Some ev -> Inputs.proc_events ev; event_loop ()
    | None -> ()
  in
  let rec op() =
    print_string (Printf.sprintf "%04x : " c.pc);
    Graphics.clear window width height;
    CPU.next c;
    event_loop ();
    Graphics.render window c.display;
  in
  let rec main_loop () =
    timers.delay <- (time_in_int() - last_time.delay);
    timers.dt <- (time_in_int() - last_time.dt);
    (* print_endline (Int.to_string timers.dt); *)
    if timers.delay > (seconds_to_nanoseconds 1)/60 then
      begin
        last_time.delay <- time_in_int();
        timers.delay <- 0;
        if c.dt > 0 then c.dt <- c.dt - 1 else ();
        if c.st > 0 then c.st <- c.st - 1 else ()
      end
    else
      ();
    if timers.dt > (seconds_to_nanoseconds 1) / 10 then
      begin
        last_time.dt <- time_in_int();
        timers.dt <- 0;
        op ();
      end
    else
      ();
    main_loop ();
  in
  main_loop ()
