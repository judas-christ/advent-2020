open Printf

let read_ints name = 
  let input = open_in name in
  let try_read () =
    try Some(input_line input) with End_of_file -> None in
  let rec loop acc = match try_read() with
    | Some s -> loop (int_of_string s :: acc)
    | None -> close_in input;
    acc; in 
  loop [];;

let find_match sum n list = 
  List.find_opt (fun other -> n + other == sum) list;;

let find_two (sum:int) (list:int list) =
  List.fold_left (fun tot curr -> match find_match sum curr list with
  | Some x -> (x, curr)
  | None -> tot) (0, 0) list;;

let find_three (sum:int) (list:int list) = 
  List.fold_left (fun tot curr -> match find_two (sum - curr) list with
  | (0,0) -> tot
  | (a,b) -> (curr, a, b))  (0,0,0) list;;

let () = 
  let ints = read_ints "/workspaces/advent2020/day1/input.txt" in
  let (a1,a2) = find_two 2020 ints in
  let answer1 = a1*a2 in
  let (b1,b2,b3) = find_three 2020 ints in
  let answer2 = b1 * b2 * b3 in
  printf "%n\n%n\n" answer1 answer2





  
  

