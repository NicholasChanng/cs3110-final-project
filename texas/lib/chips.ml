type t = int
(** Abstraction function: The integer [i] represents [i] chips in a stack. *)

type bet =
  | Trips
  | Ante
  | Four
  | Three
  | Two
  | One

let create amount =
  if amount < 0 then
    invalid_arg "Cannot create a chip stack with negative chips."
  else amount

let bet chips amount =
  if amount > chips then invalid_arg "Not enough chips for the bet."
  else chips - amount

let add chips amount =
  if amount < 0 then invalid_arg "Cannot add a negative amount of chips."
  else chips + amount

let total chips = chips

let handle_gamble gamble =
  match int_of_string_opt gamble with
  | None -> 0
  | Some x -> x

let rec prompt_for_betting chips =
  print_string "Place your bet:\n> ";
  let gamble1 = read_line () in
  let gamble = handle_gamble gamble1 in
  if gamble > chips then
    let () = print_endline "You cannot bet more than you have." in
    prompt_for_betting chips
  else if gamble < 1 then
    let () = print_endline "You must bet a positive number of chips." in
    prompt_for_betting chips
  else
    let remaining = chips - gamble in
    print_endline ("You bet " ^ string_of_int gamble ^ " chips.");
    (gamble, remaining)

let read_gamble () =
  print_string "> ";
  read_line ()

let rec print_error_and_retry error_msg bet chips optional =
  print_endline error_msg;
  ultimate_prompt_for_betting bet chips optional

(* Helper method to handle ante bet *)
and handle_ante_bet gamble chips optional bet =
  if gamble > chips / 2 then
    print_error_and_retry
      "You are making two bets so you cannot bet more than half of your chips."
      bet chips optional
  else if optional && gamble < 0 then
    print_error_and_retry "You must bet a nonnegative number of chips." bet
      chips optional
  else if (not optional) && gamble < 1 then
    print_error_and_retry "You must bet a positive number of chips." bet chips
      optional
  else
    let remaining = chips - (gamble * 2) in
    print_endline
      ("You placed a " ^ string_of_int gamble
     ^ " chip bet for each of your ante and blind bets.");
    (gamble, remaining)

and handle_general_bet gamble chips optional bet =
  if gamble > chips then
    print_error_and_retry "You cannot bet more than you have." bet chips
      optional
  else if (not optional) && gamble < 1 then
    print_error_and_retry "You must bet a positive number of chips." bet chips
      optional
  else
    let remaining = chips - gamble in
    print_endline ("You bet " ^ string_of_int gamble ^ " chips.");
    (gamble, remaining)

and ultimate_prompt_for_betting bet chips optional =
  match bet with
  | Ante ->
      print_string "Place your ante/blind bet:\n";
      let gamble1 = read_gamble () in
      let gamble = handle_gamble gamble1 in
      handle_ante_bet gamble chips optional bet
  | _ ->
      let gamble1 = read_gamble () in
      let gamble = handle_gamble gamble1 in
      handle_general_bet gamble chips optional bet

let set_bet bet_type bet remaining =
  match bet_type with
  | Four ->
      let remaining = remaining - (bet * 4) in
      (4 * bet, remaining)
  | Three ->
      let remaining = remaining - (bet * 3) in
      (3 * bet, remaining)
  | Two ->
      let remaining = remaining - (bet * 2) in
      (2 * bet, remaining)
  | One ->
      let remaining = remaining - bet in
      (bet, remaining)
  | _ -> (bet, remaining)

let rec prompt_for_set_bet_4x current_bet chips i =
  if not (i = 1) then (
    print_string "Not a valid choice; you entered 4x\n> ";
    prompt_for_set_bet current_bet chips i)
  else if current_bet * 4 <= chips then
    let bet = set_bet Four current_bet chips in
    bet
  else prompt_for_set_bet current_bet chips i

and prompt_for_set_bet_3x current_bet chips i =
  if not (i = 1) then (
    print_string "Not a valid choice; you entered 3x\n> ";
    prompt_for_set_bet current_bet chips i)
  else if current_bet * 3 <= chips then
    let bet = set_bet Three current_bet chips in
    bet
  else prompt_for_set_bet current_bet chips i

and prompt_for_set_bet_2x current_bet chips i =
  if not (i = 2) then (
    print_string "Not a valid choice; you entered 2x\n> ";
    prompt_for_set_bet current_bet chips i)
  else if current_bet * 2 <= chips then
    let bet = set_bet Two current_bet chips in
    bet
  else prompt_for_set_bet current_bet chips i

and prompt_for_set_bet_1x current_bet chips i =
  if not (i = 3) then (
    print_string "Not a valid choice; you entered 1x\n> ";
    prompt_for_set_bet current_bet chips i)
  else if current_bet <= chips then
    let bet = set_bet One current_bet chips in
    bet
  else prompt_for_set_bet current_bet chips i

and prompt_for_set_bet_check current_bet chips i =
  if i = 3 then (
    print_string "Not a valid choice; you entered check\n> ";
    prompt_for_set_bet current_bet chips i)
  else
    let bet = (current_bet, chips) in
    bet

and prompt_for_set_bet_fold current_bet chips i =
  if not (i = 3) then (
    print_string "Not a valid choice; you entered fold\n> ";
    prompt_for_set_bet current_bet chips i)
  else
    let bet = (0, 0) in
    bet

and prompt_for_set_bet current_bet chips i =
  let choice = read_line () in
  match choice with
  | "4x" -> prompt_for_set_bet_4x current_bet chips i
  | "3x" -> prompt_for_set_bet_3x current_bet chips i
  | "2x" -> prompt_for_set_bet_2x current_bet chips i
  | "1x" -> prompt_for_set_bet_1x current_bet chips i
  | "check" -> prompt_for_set_bet_check current_bet chips i
  | "fold" -> prompt_for_set_bet_fold current_bet chips i
  | _ ->
      print_string
        "This is not a valid input from the set {4x, 3x, 2x, 1x, check, fold}\n\
         > ";
      prompt_for_set_bet current_bet chips i

let blind_payout (bet : t) (score : int) : t =
  match score with
  | 10 -> bet * 500
  | 9 -> bet * 50
  | 8 -> bet * 10
  | 7 -> bet * 3
  | 6 -> 3 * bet * 2
  | 5 -> bet * 2
  | _ -> bet

let trips_payout (bet : t) (score : int) : t =
  match score with
  | 11 -> 50 * bet
  | 10 -> 40 * bet
  | 9 -> 30 * bet
  | 8 -> 8 * bet
  | 7 -> 6 * bet
  | 6 -> 5 * bet
  | 5 -> 3 * bet
  | _ -> 0
