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

module Solver2 : Solver = struct

  let trije_deli = String.split_on_char ' '
  let prvi_del = function
    | [] -> ""
    | a :: _ -> a
  let drugi_del = function
    | [] | _ :: [] -> ""
    | _ :: b :: _ -> b
  let tretji_del = function
    | [] | _ :: [] | _ :: _ :: [] -> ""
    | _ :: _ :: c :: _ -> c
  let razdeljen_prvi_del = String.split_on_char '-'
  let razdeljen_drugi_del = String.split_on_char ':'
  let vrni_prvega l = (List.nth l 0)
  let vrni_drugega l = (List.nth l 1)
  let vrni_prvega1 = function
  | [] -> ""
  | a :: _ -> a

  let prosm_delej = function
    | "0" -> 0
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    | "6" -> 6
    | "7" -> 7
    | "8" -> 8
    | "9" -> 9
    | "10" -> 10
    | "11" -> 11
    | "12" -> 12
    | "13" -> 13
    | "14" -> 14
    | "15" -> 15
    | "16" -> 16
    | "17" -> 17
    | "18" -> 18
    | "19" -> 19
    | "20" -> 20
    | _ -> failwith "Več možnosti je"

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  (* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)
  
  
  let prestevanje list = 
    let rec prestevanje_pomozna list stevilka = match list with
      | [] -> stevilka
      | x :: xs -> if x = '1' then prestevanje_pomozna xs (stevilka + 1) else prestevanje_pomozna xs stevilka
    in
    prestevanje_pomozna list 0


  let neka_druga_funkcija geslo =
    let min = prosm_delej (vrni_prvega (razdeljen_prvi_del (prvi_del (trije_deli geslo)))) in
    let max =  prosm_delej (vrni_drugega (razdeljen_prvi_del (prvi_del (trije_deli geslo)))) in
    let crka = vrni_prvega1 (razdeljen_drugi_del (drugi_del (trije_deli geslo))) in
    let crka1 = String.get crka 0 in
    let is_crka x = if (x = crka1) then '1' else ' ' in
    let ostanek = String.map (is_crka) (tretji_del (trije_deli geslo)) in
    let stevilo_pojavitev = prestevanje (explode ostanek) in
    let pravilno_geslo = if (min <= stevilo_pojavitev && stevilo_pojavitev <= max) then 1 else 0 in
    pravilno_geslo

  let naloga1 data =
    let vrstice = List.lines data in
    let nov_seznam = List.map (neka_druga_funkcija) (vrstice) in
    let sum_of_list = string_of_int (List.sum nov_seznam) in
    sum_of_list

  let razdelitev2 = String.split_on_char ':'

  let neka_druga_funkcija2 geslo =
    let min = prosm_delej (vrni_prvega (razdeljen_prvi_del (prvi_del (trije_deli geslo)))) in
    let max =  prosm_delej (vrni_drugega (razdeljen_prvi_del (prvi_del (trije_deli geslo)))) in
    let min_mesto = List.nth (explode (drugi_del (razdelitev2 geslo))) min in
    let max_mesto = List.nth (explode (drugi_del (razdelitev2 geslo))) max in
    let crka = vrni_prvega1 (razdeljen_drugi_del (drugi_del (trije_deli geslo))) in
    let crka1 = String.get crka 0 in
    let pravilno_geslo = if (min_mesto = crka1 || max_mesto = crka1) && (not (min_mesto = crka1 && max_mesto = crka1)) then 1 else 0 in
    pravilno_geslo

  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let nov_seznam = List.map (neka_druga_funkcija2) (vrstice) in
    let sum_of_list = string_of_int (List.sum nov_seznam) in
    sum_of_list

end

module Solver3 : Solver = struct

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  (* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

  let rec string n s =
    if n = 0 then "" else s ^ string (n - 1) s
  (*Vir: https://stackoverflow.com/questions/46370362/recursive-function-to-repeat-string-in-ocaml*)

  let ceu_zemljevid seznam =
    let rec ceu_zemljevid_pomozna seznam nov_seznam = match seznam with
      | [] -> nov_seznam
      | x :: xs -> ceu_zemljevid_pomozna xs (nov_seznam @ [explode (string 80 x)])
    in
    ceu_zemljevid_pomozna seznam []

  let naloga1 data =
    let vrstice = List.lines data in
    let zemljevid = ceu_zemljevid vrstice in
    let zemljevid1 = List.tl zemljevid in
    let rec preverjanje column zemlevid drevesa = match zemlevid with
      | [] -> string_of_int drevesa
      | x :: xs -> if ((List.nth x column) = '#') then (preverjanje (column+3) xs (drevesa+1)) else (preverjanje (column+3) xs (drevesa))
    in
    preverjanje 3 zemljevid1 0

  let rec preverjanje1 column zemlevid drevesa = match zemlevid with
      | [] -> drevesa
      | x :: xs -> if ((List.nth x column) = '#') then (preverjanje1 (column+1) xs (drevesa+1)) else (preverjanje1 (column+1) xs (drevesa))

  let rec preverjanje2 column zemlevid drevesa = match zemlevid with
      | [] -> drevesa
      | x :: xs -> if ((List.nth x column) = '#') then (preverjanje2 (column+3) xs (drevesa+1)) else (preverjanje2 (column+3) xs (drevesa))

  let rec preverjanje3 column zemlevid drevesa = match zemlevid with
      | [] -> drevesa
      | x :: xs -> if ((List.nth x column) = '#') then (preverjanje3 (column+5) xs (drevesa+1)) else (preverjanje3 (column+5) xs (drevesa))

  let rec preverjanje4 column zemlevid drevesa = match zemlevid with
      | [] -> drevesa
      | x :: xs -> if ((List.nth x column) = '#') then (preverjanje4 (column+7) xs (drevesa+1)) else (preverjanje4 (column+7) xs (drevesa))

  let rec preverjanje5 column zemlevid drevesa = match zemlevid with
      | [] | _ :: [] -> drevesa
      | _ :: x :: xs -> if ((List.nth x column) = '#') then (preverjanje5 (column+1) xs (drevesa+1)) else (preverjanje5 (column+1) xs (drevesa))

  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let zemljevid = ceu_zemljevid vrstice in
    let zemljevid1 = List.tl zemljevid in
    let rezultat = string_of_int ((preverjanje1 1 zemljevid1 0) * (preverjanje2 3 zemljevid1 0) * (preverjanje3 5 zemljevid1 0) * (preverjanje4 7 zemljevid1 0) * (preverjanje5 1 zemljevid1 0)) in
    rezultat
end

module Solver4 : Solver = struct

  let rec en_passport seznam nov_passport = match seznam with
          | [] -> []
          | x :: xs -> if (String.length x) = 0 then nov_passport else en_passport xs (nov_passport @ [x])

  let rec seznam_brez_prvih_n_elementov seznam n = if n <= 0 then seznam else seznam_brez_prvih_n_elementov (List.tl seznam) (n-1)

  let rec seznam_passportov podatki nov_seznam = match podatki with
    | [] -> nov_seznam
    | _ -> seznam_passportov (seznam_brez_prvih_n_elementov (podatki) ((List.length (en_passport podatki [])) + 1)) (nov_seznam @ [ String.concat " " (en_passport podatki []) ])

  let rec sezstr_to_sezsez sezstr nov_seznam = match sezstr with
    | [] -> nov_seznam
    | x :: xs -> sezstr_to_sezsez xs (nov_seznam @ [String.split_on_char ' ' x])

  let seznam_locen_s_podpicjem seznam = String.split_on_char ':' (String.concat ":" seznam)

  let seznam_vsebuje_cid seznam = List.mem "cid" seznam

  let rec test sezsez stevilka = match sezsez with
    | [] -> string_of_int stevilka
    | x :: xs -> if ((List.length x) == 8) || (((List.length x) == 7) && (not (seznam_vsebuje_cid (seznam_locen_s_podpicjem x)))) then test xs (stevilka+1) else test xs stevilka

  let naloga1 data =
    let vrstice = String.split_on_char '\n' data in
    let seznam_passport_stringov = seznam_passportov vrstice [] in
    let seznam_passport_seznamov = sezstr_to_sezsez seznam_passport_stringov [] in
    let rezultat = test seznam_passport_seznamov 0 in
    rezultat

  let rec sezstr_to_sezsez2 sezstr nov_seznam = match sezstr with
  | [] -> nov_seznam
  | x :: xs -> sezstr_to_sezsez2 xs (nov_seznam @ [String.split_on_char ':' x])

  let rec sezsez_to_sezsezsez sezsez sezsezsez = match sezsez with
    | [] -> sezsezsez
    | x :: xs -> sezsez_to_sezsezsez xs (sezsezsez @ [(sezstr_to_sezsez2 x [])])

  let test_je_digit x = match x with
    | '0' -> true
    | '1' -> true
    | '2' -> true
    | '3' -> true
    | '4' -> true
    | '5' -> true
    | '6' -> true
    | '7' -> true
    | '8' -> true
    | '9' -> true
    | _ -> false

  let test_je_letter x = match x with
    | 'a' -> true
    | 'b' -> true
    | 'c' -> true
    | 'd' -> true
    | 'e' -> true
    | 'f' -> true
    | _ -> false

  let test_je_c x = match x with
    | 'c' -> true
    | _ -> false

  let test_je_m x = match x with
    | 'm' -> true
    | _ -> false

  let test_je_i x = match x with
    | 'i' -> true
    | _ -> false

  let test_je_n x = match x with
    | 'n' -> true
    | _ -> false

  let zmanjkuje_mi_idej1 string =
    let list = String.split_on_char 'c' string in
    let zmanjkuje_mi_idej2 = if (150 <= (int_of_string (List.nth list 0))) && ((int_of_string (List.nth list 0)) <= 193) then true else false in
    zmanjkuje_mi_idej2
  
  let zmanjkuje_mi_idej3 string =
    let list = String.split_on_char 'i' string in
    let zmanjkuje_mi_idej4 = if (59 <= (int_of_string (List.nth list 0))) && ((int_of_string (List.nth list 0)) <= 76) then true else false in
    zmanjkuje_mi_idej4

  let posebej_test_za_height y dolzina = match dolzina with
    | 5 -> if (test_je_digit (y.[0])) && (test_je_digit (y.[2])) && (test_je_digit (y.[1])) && (test_je_c (y.[3])) && (test_je_m (y.[4])) then zmanjkuje_mi_idej1 y else false
    | 4 -> if (test_je_digit (y.[0])) && (test_je_digit (y.[1])) && (test_je_i (y.[2])) && (test_je_n (y.[3])) then zmanjkuje_mi_idej3 y else false
    | _ -> false

  let testiranje_podatkov x y = match x with
    | "byr" -> if ((String.length y) = 4) && (1920 <= (int_of_string y)) && ((int_of_string y) <= 2002) then true else false
    | "iyr" -> if ((String.length y) = 4) && (2010 <= (int_of_string y)) && ((int_of_string y) <= 2020) then true else false
    | "eyr" -> if ((String.length y) = 4) && (2020 <= (int_of_string y)) && ((int_of_string y) <= 2030) then true else false
    | "hgt" -> posebej_test_za_height y (String.length y)
    | "hcl" -> if (y.[0] = '#') && ((String.length y) = 7) && ((test_je_letter (y.[1])) || (test_je_digit (y.[1])))
                   && ((test_je_letter (y.[2])) || (test_je_digit (y.[2]))) && ((test_je_letter (y.[3])) || (test_je_digit (y.[3])))
                   && ((test_je_letter (y.[4])) || (test_je_digit (y.[4]))) && ((test_je_letter (y.[5])) || (test_je_digit (y.[5])))
                   && ((test_je_letter (y.[6])) || (test_je_digit (y.[6])))
                   then true else false
    | "ecl" -> if (y = "amb") || (y = "blu") || (y = "brn") || (y = "gry") || (y = "grn") || (y = "hzl") || (y = "oth") then true else false
    | "pid" -> if ((String.length y) = 9) && (test_je_digit (y.[0])) && (test_je_digit (y.[2])) && (test_je_digit (y.[3])) && (test_je_digit (y.[4]))
                   && (test_je_digit (y.[5])) && (test_je_digit (y.[6])) && (test_je_digit (y.[7])) && (test_je_digit (y.[8])) && (test_je_digit (y.[1]))
                   then true else false
    | "cid" -> true
    | _ -> false

  let testiranje_validnosti par_v_obliki_seznama = match par_v_obliki_seznama with
    | [] | _ :: [] -> failwith "Nekinedela"
    | _ -> testiranje_podatkov (List.nth par_v_obliki_seznama 0) (List.nth par_v_obliki_seznama 1)

  let rec test_ki_nardi_seznam sezsez nov_seznam = match sezsez with
    | [] -> nov_seznam
    | x :: xs -> if ((List.length x) == 8) || (((List.length x) == 7) && (not (seznam_vsebuje_cid (seznam_locen_s_podpicjem x)))) then test_ki_nardi_seznam xs (nov_seznam @ [x]) else test_ki_nardi_seznam xs nov_seznam

  let rec se_en_test sezsezsez stevilka = match sezsezsez with
    | [] -> string_of_int stevilka
    | x1 :: rest1 -> let rec se_en_test2 sezsez = match sezsez with
                    | [] -> se_en_test rest1 (stevilka + 1)
                    | x2 :: rest2 -> if testiranje_validnosti x2 then se_en_test2 rest2 else se_en_test rest1 stevilka
                    in 
                    se_en_test2 x1

  let naloga2 data _part1 =
    let vrstice = String.split_on_char '\n' data in
    let seznam_passport_stringov = seznam_passportov vrstice [] in
    let seznam_passport_seznamov = sezstr_to_sezsez seznam_passport_stringov [] in
    let tapravi = test_ki_nardi_seznam seznam_passport_seznamov [] in
    let sezsezsez = sezsez_to_sezsezsez tapravi [] in
    let rezultat = se_en_test sezsezsez 0 in
    rezultat

end


(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
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
