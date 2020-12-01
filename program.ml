let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct

  let sum_is_2020 x y = if (x + y = 2020) then true else false

  let naloga1 data = 
    let vrstice = List.lines data in
    let nekej = List.int_list vrstice in
    let rec preverjanje seznam = match seznam with
      | [] -> "Napaka"
      | x :: rest ->
          let rec preverjanje2 x ostalo = match ostalo with
            | [] -> preverjanje rest
            | y :: ostalo when (sum_is_2020 x y) -> string_of_int (x * y)
            | y :: ostalo -> preverjanje2 x ostalo
          in
          preverjanje2 x rest
    in
    preverjanje nekej

  let sum_of_three_is_2020 x y z = if (x + y + z = 2020) then true else false

  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let nekej = List.int_list vrstice in
    let rec preverjanje seznam = match seznam with
      | [] -> "Napaka"
      | x1 :: rest1 ->
          let rec preverjanje2 x1 ostalo = match ostalo with
            | [] -> preverjanje rest1
            | x2 :: rest2 -> 
                let rec preverjanje3 x2 ostalo = match ostalo with
                  | [] -> preverjanje2 x1 rest2
                  | x3 :: rest3 -> if (sum_of_three_is_2020 x1 x2 x3) then string_of_int (x1 * x2 * x3) else preverjanje3 x2 rest3
                in
                preverjanje3 x2 rest2
          in
          preverjanje2 x1 rest1
    in
    preverjanje nekej
end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "1" -> (module Solver1)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()
