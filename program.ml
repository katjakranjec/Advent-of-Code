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

module Solver5 : Solver = struct

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  (* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

  let rec test_plane seznam nov_seznam = match seznam with
    | [] -> nov_seznam
    | x1 :: rest1 -> let rec seat_ID seznam row_min row_max column_min column_max = match seznam with
                      | [] -> test_plane rest1 (nov_seznam @ [((row_min - 1) * 8) + (column_min -1)])
                      | x2 :: rest2 -> let fblr znak = match znak with
                                        | 'F' -> seat_ID rest2 row_min (row_max - (((row_max - row_min) + 1) / 2)) column_min column_max
                                        | 'B' -> seat_ID rest2 (row_min + (((row_max - row_min) + 1) / 2)) row_max column_min column_max
                                        | 'L' -> seat_ID rest2 row_min row_max column_min (column_max - (((column_max - column_min) + 1) / 2))
                                        | 'R' -> seat_ID rest2 row_min row_max (column_min + (((column_max - column_min) + 1) / 2)) column_max
                                        | _ -> failwith "Neki ne dela"
                                        in
                                        fblr x2
                      in
                      seat_ID (explode x1) 1 128 1 8

  let rec max_ID seznam maximum = match seznam with
    | [] -> maximum
    | x :: xs -> if (x >= maximum) then max_ID xs x else max_ID xs maximum


  let naloga1 data =
    let vrstice = List.lines data in
    let seznam_stevilk = test_plane vrstice [] in
    let rezultat = string_of_int (max_ID seznam_stevilk 0) in
    rezultat
  
  let rec keri_IDi_manjkajo seznam_idev row1 manjkajo1 = match row1 with
    | 1 -> manjkajo1
    | _ -> let rec keri_columni_manjkajo seznam_idev row column manjkajo = match column with
            | 0 -> keri_IDi_manjkajo seznam_idev (row-1) manjkajo
            | _ -> if (List.mem (((row - 1) * 8) + (column - 1)) seznam_idev) 
                   then keri_columni_manjkajo seznam_idev row (column - 1) manjkajo
                   else keri_columni_manjkajo seznam_idev row (column - 1) (manjkajo @ [((row - 1) * 8) + (column - 1)])
           in
           keri_columni_manjkajo seznam_idev row1 8 manjkajo1

  let rec najdi_tapravga seznam_idev manjkajo = match manjkajo with
    | [] -> failwith "Neki ne dela"
    | x :: xs -> if (List.mem (x + 1) seznam_idev) && (List.mem (x + 1) seznam_idev) then x else najdi_tapravga seznam_idev xs
  
  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let seznam_seatov = test_plane vrstice [] in
    let rezultat2 = string_of_int (najdi_tapravga seznam_seatov (keri_IDi_manjkajo seznam_seatov 127 [])) in
    rezultat2

end

module Solver6 : Solver = struct

  let rec one_group seznam new_group = match seznam with
        | [] -> []
        | x :: xs -> if (String.length x) = 0 then new_group else one_group xs (new_group @ [x])

  let rec seznam_brez_prvih_n_elementov seznam n = if n <= 0 then seznam else seznam_brez_prvih_n_elementov (List.tl seznam) (n-1)

  let rec seznam_group podatki nov_seznam = match podatki with
    | [] -> nov_seznam
    | _ -> seznam_group (seznam_brez_prvih_n_elementov (podatki) ((List.length (one_group podatki [])) + 1)) (nov_seznam @ [ String.concat "" (one_group podatki []) ])

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
    (* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

  let vprasanja = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
  
  let list_characterjev string = if (String.length string) = 1 then [string.[0]] else explode string

  let rec test vprasanja seznam nov_seznam = match seznam with
    | [] -> nov_seznam
    | x1 :: xs1 -> let rec test2 vprasanja n yesi odgovori = match n with
                  | 26 -> test vprasanja xs1 (nov_seznam @ [yesi])
                  | _ -> if (List.mem (List.nth vprasanja n) odgovori) then test2 vprasanja (n+1) (yesi+1) odgovori else test2 vprasanja (n+1) (yesi) odgovori
                  in
                  test2 vprasanja 0 0 (list_characterjev x1)

  let naloga1 data =
    let vrstice = String.split_on_char '\n' data in
    let seznam_skupin = seznam_group vrstice [] in
    let seznam_yesev = test vprasanja seznam_skupin [] in
    let rezultat = string_of_int (List.sum seznam_yesev) in
    rezultat

  let rec one_group seznam new_group = match seznam with
        | [] -> []
        | x :: xs -> if (String.length x) = 0 then new_group else one_group xs (new_group @ [x])

  let rec seznam_brez_prvih_n_elementov seznam n = if n <= 0 then seznam else seznam_brez_prvih_n_elementov (List.tl seznam) (n-1)

  let rec seznam_group2 podatki nov_seznam = match podatki with
    | [] -> nov_seznam
    | _ -> seznam_group2 (seznam_brez_prvih_n_elementov (podatki) ((List.length (one_group podatki [])) + 1)) (nov_seznam @ [ one_group podatki [] ])

  let vprasanja = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

  let rec test3 vprasanja seznam nov_seznam = match seznam with
    | [] -> nov_seznam
    | x1 :: xs1 -> let rec test4 vprasanja n yesi1 odgovori = match n with
                    | 26 -> test3 vprasanja xs1 (nov_seznam @ [yesi1])
                    | _ -> let rec test5 crka odgovori = match odgovori with
                            | [] -> test4 vprasanja (n+1) (yesi1 + 1) odgovori
                            | x2 :: xs2 -> if not (List.mem crka (explode x2)) then test4 vprasanja (n+1) yesi1 odgovori else test5 crka xs2
                            in
                            test5 (List.nth vprasanja n) x1
                    in
                    test4 vprasanja 0 0 x1

  let naloga2 data _part1 = 
    let vrstice = String.split_on_char '\n' data in
    let seznam_skupin2 = seznam_group2 vrstice [] in
    let seznam_yesev2 = test3 vprasanja seznam_skupin2 [] in
    let rezultat2 = string_of_int (List.sum seznam_yesev2) in
    rezultat2

end

module Solver7 : Solver = struct

  let loceno_s_presledki niz = String.split_on_char ' ' niz

  let rec seznam_brez_prvih_n_elementov seznam n = if n <= 0 then seznam else seznam_brez_prvih_n_elementov (List.tl seznam) (n-1)

  let rec eno_navodilo seznam1 nov_seznam1 = match seznam1 with
    | [] -> nov_seznam1
    | x1 :: rest1 -> let rec del_navodila seznam2 nov_seznam2 = match seznam2 with
                      | [] -> failwith "Do tega nebi smel pridt"
                      | x :: _ -> if x = "contain" || x = "bags," || x = "bag," || x = "bags." || x = "bag."
                                  then eno_navodilo (seznam_brez_prvih_n_elementov seznam1 ((List.length nov_seznam2) + 1)) (nov_seznam1 @ [nov_seznam2])
                                  else del_navodila (seznam_brez_prvih_n_elementov seznam2 1) (nov_seznam2 @ [x])
                      in
                      del_navodila seznam1 []

  let rec seznam_navodil podatki nov_seznam = match podatki with
    | [] -> nov_seznam
    | x :: xs -> seznam_navodil xs (nov_seznam @ [eno_navodilo (loceno_s_presledki x) [] ])

  let rec izbira_navodila lastnost barva navodila = match navodila with
    | [] -> failwith "Ze prej bi mogla najdt tapravo"
    | x :: xs -> if (List.mem lastnost (List.hd x)) && (List.mem barva (List.hd x)) then x else izbira_navodila lastnost barva xs

  let rec a_lah_vsebuje_sajni_gold_vrecko navodila vrecka = match vrecka with
    | [] | _ :: [] -> failwith "Ne bi smel do tega pridt"
    | x1 :: xs1 -> let rec funkcija1 kar_ostane n = match n with
                    | (-1) -> false
                    | _ -> let funkcija x3 = if (List.mem "shiny" x3) && (List.mem "gold" x3) then true
                                                      else if (List.mem "no" x3) then false
                                                      else a_lah_vsebuje_sajni_gold_vrecko navodila (izbira_navodila (List.nth x3 1) (List.nth x3 2) navodila)
                           in
                           ((funkcija (List.nth kar_ostane n)) || (funkcija1 kar_ostane (n-1)))
                   in
                   funkcija1 xs1 ((List.length xs1)-1)

  let rec stetje seznam_vreck navodila stevilo = match seznam_vreck with
    | [] -> string_of_int stevilo
    | x :: xs -> if (a_lah_vsebuje_sajni_gold_vrecko navodila x) then stetje xs navodila (stevilo + 1)
                 else stetje xs navodila stevilo
                  
                 

  let naloga1 data =
    let vrstice = List.lines data in
    let rezultat = stetje (seznam_navodil vrstice []) (seznam_navodil vrstice []) 0 in
    rezultat

  let izlocitev_podatkov del_vrece = match del_vrece with
    | [] | _ :: [] | _ :: _ :: [] -> failwith "Upejmo, da se to ne zgodi"
    | _ :: lastnost :: barva :: _ -> (lastnost, barva)

  let nova_vreca (lastnost, barva) navodila = izbira_navodila lastnost barva navodila

  let rec gremo_pogledat_v_vreco vreca navodila = match vreca with
    | [] -> failwith "Nekej"
    | x :: xs when (List.mem "no" (List.hd xs)) -> 1
    | x :: xs -> let rec kaj_vsebuje_vreca vsebina n = match n with
                  | (-1) -> 1
                  | _ -> ((int_of_string (List.hd (List.nth vsebina n))) * (gremo_pogledat_v_vreco (nova_vreca (izlocitev_podatkov (List.nth vsebina n)) navodila) navodila)) + (kaj_vsebuje_vreca vsebina (n-1))
                  in
                  kaj_vsebuje_vreca xs ((List.length xs)-1)

  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let sajni_vreca = izbira_navodila "shiny" "gold" (seznam_navodil vrstice []) in
    let rezultat = string_of_int ((gremo_pogledat_v_vreco (sajni_vreca) (seznam_navodil vrstice [])) - 1) in
    rezultat


end

module Solver8 : Solver = struct

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
    (* Vir: https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

  let rec pobegni list nov_list = match list with
    | [] -> nov_list
    | x :: xs -> pobegni xs (nov_list @ [Char.escaped x])

  let uredi_vrstico1 vrstica = String.split_on_char ' ' vrstica

  let uredi_vrstico2 vrstica = []
                              @ [List.hd vrstica] 
                              @ [Char.escaped ((List.nth vrstica 1).[0])] 
                              @ [String.concat "" (pobegni (List.tl (explode (List.nth vrstica 1))) [])]

  let rec prvi_krog vrstica seznam_vseh kjer_smo_ze_bli kje_smo acc = match vrstica with
    | x :: xs when (List.mem kje_smo kjer_smo_ze_bli) -> acc
    | x :: y :: z :: [] -> 
      let kaj_narest operacija plusminus stevilka = match operacija with
        | "acc" -> if plusminus = "+" 
                    then prvi_krog (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka))
                    else prvi_krog (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc - (int_of_string stevilka))
        | "jmp" -> if plusminus = "+"
                   then prvi_krog (List.nth seznam_vseh (kje_smo + (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc
                   else prvi_krog (List.nth seznam_vseh (kje_smo - (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc
        | "nop" -> prvi_krog (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc
        | _ -> failwith "To bi mogle bit edine možnosti"
      in
      kaj_narest x y z
    | _ -> failwith "Do tega upejmo nau pršlo"

  let rec urejene_vrstice neurejene urejene = match neurejene with
    | [] -> urejene
    | x :: xs -> urejene_vrstice xs (urejene @ [(uredi_vrstico2 (uredi_vrstico1 x))])

  let naloga1 data =
    let vrstice = List.lines data in
    let prva_vrstica = uredi_vrstico2 (uredi_vrstico1 (List.hd vrstice)) in
    let rezultat = string_of_int (prvi_krog prva_vrstica (urejene_vrstice vrstice []) [] 0 0) in
    rezultat

  let rec sesuvanje vrstica seznam_vseh kjer_smo_ze_bli kje_smo acc n = match vrstica with
      | x :: xs when (List.mem kje_smo kjer_smo_ze_bli) -> false
      | x :: y :: z :: [] when (kje_smo = n) -> 
        let kaj_narest1 operacija plusminus stevilka = match operacija with
          | "acc" -> if (kje_smo + 1) >= (List.length seznam_vseh) then true else
                    if plusminus = "+" 
                    then sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n
                    else sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc - (int_of_string stevilka)) n
          | "nop" -> if (plusminus = "+") && ((kje_smo + (int_of_string stevilka)) >= (List.length seznam_vseh)) then true else
                    if (plusminus = "-") && ((kje_smo - (int_of_string stevilka)) < 0) then true else
                    if plusminus = "+"
                    then sesuvanje (List.nth seznam_vseh (kje_smo + (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n
                    else sesuvanje (List.nth seznam_vseh (kje_smo - (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n
          | "jmp" -> if (kje_smo + 1) >= (List.length seznam_vseh) then true 
                    else sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
          | _ -> failwith "To bi mogle bit edine možnosti"
        in
        kaj_narest1 x y z
      | x :: y :: z :: [] -> 
        let kaj_narest2 operacija plusminus stevilka = match operacija with
          | "acc" -> if (kje_smo + 1) >= (List.length seznam_vseh) then true else
                    if plusminus = "+" 
                    then sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n
                    else sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc - (int_of_string stevilka)) n
          | "jmp" -> if (plusminus = "+") && ((kje_smo + (int_of_string stevilka)) >= (List.length seznam_vseh)) then true else
                    if (plusminus = "-") && ((kje_smo - (int_of_string stevilka)) < 0) then true else
                    if plusminus = "+"
                    then sesuvanje (List.nth seznam_vseh (kje_smo + (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n
                    else sesuvanje (List.nth seznam_vseh (kje_smo - (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n
          | "nop" -> if (kje_smo + 1) >= (List.length seznam_vseh) then true 
                    else sesuvanje (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
          | _ -> failwith "To bi mogle bit edine možnosti"
        in
        kaj_narest2 x y z
      | _ -> failwith "Do tega upejmo nau pršlo"

  let rec prvi_krog2 vrstica seznam_vseh kjer_smo_ze_bli kje_smo acc n = match vrstica with
    | x :: y :: z :: [] when ((kje_smo >= (List.length seznam_vseh)) || (kje_smo < 0)) -> acc
    | x :: y :: z :: [] when (kje_smo = n) ->
      let kaj_narest operacija plusminus stevilka = match operacija with
        | "acc" -> if (kje_smo + 1) >= (List.length seznam_vseh) 
                    then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n else
                    if plusminus = "+" 
                    then prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n
                    else prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc - (int_of_string stevilka)) n
        | "nop" -> if (plusminus = "+") && ((kje_smo + (int_of_string stevilka)) >= (List.length seznam_vseh)) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n else
                  if (plusminus = "-") && ((kje_smo - (int_of_string stevilka)) < 0) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n else
                  if plusminus = "+"
                  then prvi_krog2 (List.nth seznam_vseh (kje_smo + (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n
                  else prvi_krog2 (List.nth seznam_vseh (kje_smo - (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n
        | "jmp" -> if (kje_smo + 1) >= (List.length seznam_vseh) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
                  else prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
        | _ -> failwith "To bi mogle bit edine možnosti"
      in
      kaj_narest x y z
    | x :: y :: z :: [] -> 
      let kaj_narest2 operacija plusminus stevilka = match operacija with
        | "acc" -> if (kje_smo + 1) >= (List.length seznam_vseh) 
                    then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n else
                    if plusminus = "+" 
                    then prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc + (int_of_string stevilka)) n
                    else prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) (acc - (int_of_string stevilka)) n
        | "jmp" -> if (plusminus = "+") && ((kje_smo + (int_of_string stevilka)) >= (List.length seznam_vseh)) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n else
                  if (plusminus = "-") && ((kje_smo - (int_of_string stevilka)) < 0) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n else
                  if plusminus = "+"
                  then prvi_krog2 (List.nth seznam_vseh (kje_smo + (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + (int_of_string stevilka)) acc n
                  else prvi_krog2 (List.nth seznam_vseh (kje_smo - (int_of_string stevilka))) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo - (int_of_string stevilka)) acc n
        | "nop" -> if (kje_smo + 1) >= (List.length seznam_vseh) 
                  then prvi_krog2 (vrstica) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
                  else prvi_krog2 (List.nth seznam_vseh (kje_smo + 1)) seznam_vseh (kjer_smo_ze_bli @ [kje_smo]) (kje_smo + 1) acc n
        | _ -> failwith "To bi mogle bit edine možnosti"
      in
      kaj_narest2 x y z
    | _ -> failwith "Do tega upejmo nau pršlo"

  let rec kdaj_se_sesuje seznam seznam_vseh n = match seznam with
    | [] -> failwith "Že prej bi se mogl zgodit"
    | x :: xs when ((List.length x) = 3) -> let ali_se_sesuje operacija = match operacija with
                                            | "acc" -> kdaj_se_sesuje xs seznam_vseh (n+1)
                                            | "jmp" | "nop" -> if (sesuvanje (List.hd seznam_vseh) seznam_vseh [] 0 0 n)
                                                              then n
                                                              else kdaj_se_sesuje xs seznam_vseh (n+1)
                                            | _ -> failwith "Sj se to nau zgodil"
                                            in
                                            ali_se_sesuje (List.hd x)
    | _ -> failwith "Očitno obstajajo vrstice k niso tolk dolge"

  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let prva_vrstica = uredi_vrstico2 (uredi_vrstico1 (List.hd vrstice)) in
    let vse_vrstice = urejene_vrstice vrstice [] in
    let zamenana_vrstica = string_of_int (kdaj_se_sesuje (urejene_vrstice vrstice []) (urejene_vrstice vrstice []) 0) in
    let rezultat2 = string_of_int (prvi_krog2 prva_vrstica vse_vrstice [] 0 0 (int_of_string zamenana_vrstica)) in
    rezultat2

end

module Solver9 : Solver = struct

  let rec seenafunkcija1 seznam nov_seznam stevilka n kje_smo mesto_stevilke = match seznam with
    | [] -> nov_seznam
    | x :: xs -> if (kje_smo >= (mesto_stevilke - n)) && (kje_smo < mesto_stevilke) then seenafunkcija1 xs (nov_seznam @ [x]) stevilka n (kje_smo + 1) mesto_stevilke
                  else seenafunkcija1 xs (nov_seznam) stevilka n (kje_smo + 1) mesto_stevilke

  let rec seenafunkcija seznam_25 stevilka = match seznam_25 with
    | [] -> true
    | x1 :: xs1 -> let rec funkcija prva_stevilka preostanek = match preostanek with
                    | [] -> seenafunkcija xs1 stevilka
                    | x2 :: xs2 -> if ((int_of_string x1) + (int_of_string x2) = stevilka) then false else funkcija prva_stevilka xs2
                    in
                    funkcija x1 xs1

  let rec pregledovanje seznam mesto = match mesto with
    | 1000 -> failwith "že prej bi mogl najdt"
    | _ -> let poglejmo_jih_25_za_nazaj seznam_prejsnjih stevilka = 
              if seenafunkcija seznam_prejsnjih stevilka
              then stevilka else pregledovanje seznam (mesto+1)
            in
            poglejmo_jih_25_za_nazaj (seenafunkcija1 seznam [] (int_of_string (List.nth seznam mesto)) 25 0 mesto) (int_of_string (List.nth seznam mesto))

  let rec seznam_intov seznam nov_seznam = match nov_seznam with
    | [] -> nov_seznam
    | x :: xs -> seznam_intov xs (nov_seznam @ [x])

  let naloga1 data =
    let vrstice = List.lines data in
    let rezultat = string_of_int (pregledovanje vrstice 25) in
    rezultat

  let rec contiguous_set seznam rezultat = match seznam with
    | [] -> failwith "Že prej bi mogl pridt"
    | x :: xs -> let rec funkcija preostanek sestevek nov_seznam = match preostanek with
                  | [] -> contiguous_set xs rezultat
                  | x2 :: xs2 -> if ((sestevek + (int_of_string x2)) = rezultat) then (nov_seznam @ [x2]) else
                                  if sestevek > rezultat then contiguous_set xs rezultat
                                  else funkcija xs2 (sestevek + (int_of_string x2)) (nov_seznam @ [x2])
                  in
                  funkcija xs (int_of_string x) [x]

  let rec najdi_max seznam = match seznam with
    | [] -> 0
    | x :: xs -> max (int_of_string x) (najdi_max xs)

  let rec najdi_min seznam = match seznam with
    | [] -> 0
    | x :: xs -> min (int_of_string x) (najdi_max xs)


  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let zaporedne_stevilke = contiguous_set vrstice (int_of_string _part1) in
    let max = (najdi_max zaporedne_stevilke) in
    let min = (najdi_min zaporedne_stevilke) in
    let rezultat2 = string_of_int (min + max) in
    rezultat2

end

module Solver10 : Solver = struct

  let rec po_velikosti seznam nov_seznam n =
    if ((List.length seznam) + 1) = (List.length nov_seznam) then nov_seznam else
    if (List.mem (string_of_int n) seznam) then po_velikosti seznam (nov_seznam @ [n]) (n+1) else po_velikosti seznam nov_seznam (n+1)

  let rec razlike seznam razlika1 razlika3 = match seznam with
    | [] | _ :: [] -> razlika1 * razlika3
    | x :: y :: rest -> if (y-x) = 1 then razlike (y :: rest) (razlika1 + 1) razlika3 else
                        if (y-x) = 3 then razlike (y :: rest) razlika1 (razlika3 + 1) else
                        razlike (y :: rest) razlika1 razlika3

  let naloga1 data =
    let vrstice = List.lines data in
    let urejeni = po_velikosti vrstice [0] 1 in
    let rezultat = string_of_int (razlike urejeni 0 1) in
    rezultat

  let rec funkcija_moznosti element ostali moznosti1 = match ostali with
    | [] -> moznosti1
    | x :: xs -> if (x = element + 1) || (x = element + 2) || (x = element + 3) then funkcija_moznosti element xs (moznosti1 @ [x])
                  else funkcija_moznosti element xs moznosti1
 
  (*let rec funkcija_preveri seznam element =
    let moznosti moznosti2 = match moznosti2 with
            | [] -> 0
            | x :: [] -> (funkcija_preveri seznam x)
            | x :: y :: [] -> (funkcija_preveri seznam x) + (funkcija_preveri seznam y) + 1
            | x :: y :: z :: [] -> (funkcija_preveri seznam x) + (funkcija_preveri seznam y) + (funkcija_preveri seznam z) + 2
            | _ -> failwith "Ne bi smel bit drugih moznosti"
            in
            moznosti (funkcija_moznosti element seznam [])*)

  let rec ali_je_ze_izracunan seznam element = match seznam with
    | [] -> []
    | x :: xs -> if (List.hd x) = element then x else ali_je_ze_izracunan xs element

  let rec obrni_seznam seznam nov_seznam = match seznam with
    | [] -> nov_seznam
    | x :: xs -> obrni_seznam xs (x :: nov_seznam)

  let rec funkcija_preveri2 seznam cel_seznam seznam_ze_izracunanih = match seznam with
    | [] -> seznam_ze_izracunanih
    | element :: xs -> let rec funkcija_preveri3 seznam element seznam_ze_izracunanih =
                        let moznosti3 moznosti3 = match moznosti3 with
                                | [] -> 0
                                | x :: _  when (List.length (ali_je_ze_izracunan seznam_ze_izracunanih element)) = 2 ->
                                    List.nth (ali_je_ze_izracunan seznam_ze_izracunanih element) 1
                                | x :: [] -> (funkcija_preveri3 seznam x seznam_ze_izracunanih)
                                | x :: y :: [] -> (funkcija_preveri3 seznam x seznam_ze_izracunanih) + (funkcija_preveri3 seznam y seznam_ze_izracunanih) + 1
                                | x :: y :: z :: [] -> (funkcija_preveri3 seznam x seznam_ze_izracunanih) + (funkcija_preveri3 seznam y seznam_ze_izracunanih) + (funkcija_preveri3 seznam z seznam_ze_izracunanih) + 2
                                | _ -> failwith "Ne bi smel bit drugih moznosti"
                                in
                                moznosti3 (funkcija_moznosti element seznam [])
                        in
                        funkcija_preveri2 xs cel_seznam (seznam_ze_izracunanih @ [[element; funkcija_preveri3 cel_seznam element seznam_ze_izracunanih]])

  (*let upejmo_da_delujoca_funkcija seznam izracunani*)
  
  let naloga2 data _part1 =
    let vrstice = List.lines data in
    let urejeni = po_velikosti vrstice [0] 1 in
    let obrnjeni = obrni_seznam urejeni [] in
    let seznam_izracunanih = (funkcija_preveri2 obrnjeni obrnjeni []) in
    let rezultat2 = string_of_int ((List.nth (List.nth seznam_izracunanih ((List.length obrnjeni)-1)) 1) + 1) in
    rezultat2

end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "7" -> (module Solver7)
  | "8" -> (module Solver8)
  | "9" -> (module Solver9)
  | "10"-> (module Solver10)
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
