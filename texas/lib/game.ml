(* Helper functions for dealing cards *)
let deal_flop deck =
  let first_tuple = Deck.draw deck in
  let second_tuple = Deck.draw (snd first_tuple) in
  let third_tuple = Deck.draw (snd second_tuple) in
  ([ fst first_tuple; fst second_tuple; fst third_tuple ], snd third_tuple)

let deal_turn deck flop =
  let first_tuple = Deck.draw deck in
  (flop @ [ fst first_tuple ], snd first_tuple)

let deal_river deck turn =
  let first_tuple = Deck.draw deck in
  (turn @ [ fst first_tuple ], snd first_tuple)

let assign_score evaluation =
  match evaluation with
  | "Royal Flush" -> 11
  | "Straight Flush" -> 10
  | "Four of a Kind" -> 9
  | "Full House" -> 8
  | "Flush" -> 7
  | "Straight" -> 6
  | "Three of a Kind" -> 5
  | "Two Pair" -> 4
  | "One Pair" -> 3
  | "One of a Kind" -> 2
  | _ -> 1

let print_loss_message loss remaining_chips =
  print_endline
    ("You lost " ^ string_of_int loss ^ " chips.\nYou have "
    ^ string_of_int remaining_chips
    ^ " chips remaining.")

let print_win_message win remaining_chips =
  print_endline
    ("You won " ^ string_of_int win ^ " chips.\nYou have "
    ^ string_of_int remaining_chips
    ^ " chips remaining.")

let calculate_loss dealer_score player_score play ante blind trips =
  if dealer_score < 3 then
    let loss = Chips.total (fst play) + Chips.total trips + Chips.total blind in
    (loss, snd play, false)
  else
    let trips_payout = Chips.total (Chips.trips_payout trips player_score) in
    let loss =
      Chips.total (fst play)
      - trips_payout + Chips.total blind + Chips.total ante + Chips.total trips
    in
    if loss < 0 then
      let chips = Chips.add (snd play) (-loss) in
      (-loss, chips, true)
    else (loss, snd play, false)

let process_loss dealer_score player_score play ante blind trips =
  let loss, chips, won =
    calculate_loss dealer_score player_score play ante blind trips
  in
  if won then print_win_message loss (Chips.total chips)
  else if loss > 0 then print_loss_message loss (Chips.total chips);
  chips

let process_win player_score play ante blind trips =
  let win =
    Chips.total (Chips.blind_payout blind player_score)
    + Chips.total (Chips.trips_payout trips player_score)
    + (Chips.total ante * 2)
    + (Chips.total (fst play) * 2)
  in
  let gain =
    win - Chips.total blind - Chips.total trips - Chips.total ante
    - Chips.total (fst play)
  in
  let chips = Chips.add (snd play) win in
  let () =
    print_endline
      ("Congrats! You win!\nYou gained " ^ string_of_int gain
     ^ " chips.\nYou now have "
      ^ string_of_int (Chips.total chips)
      ^ " chips.")
  in
  chips

let process_push player_score play ante blind trips =
  let push =
    Chips.total (fst play)
    + Chips.total (Chips.trips_payout trips player_score)
    + Chips.total blind + Chips.total ante
    + Chips.total (Chips.blind_payout blind player_score)
  in
  let chips = Chips.create (Chips.total (snd play) + push) in
  let () =
    print_endline
      ("You pushed.\nYou have "
      ^ string_of_int (Chips.total chips)
      ^ " chips left")
  in
  chips

let print_known_hands player community =
  let () = print_endline "Below is your hand:" in
  let () = Deck.print_cards (Deck.to_deck player) in
  let () = print_endline "Below are the community cards:" in
  let () = Deck.print_cards (Deck.to_deck community) in
  print_endline ""

let rec game_loop deck chips player dealer =
  let chips = Chips.ultimate_prompt_for_betting Chips.Ante chips false in
  let ante = Chips.total (fst chips) in
  let blind = Chips.total (fst chips) in
  print_string "Place your trips bet (optional, enter 0 to refuse):\n> ";
  let chips = Chips.ultimate_prompt_for_betting Chips.Trips (snd chips) true in
  let trips = fst chips in
  let deck = Player.deal_to_player (Deck.shuffle deck) player in
  let deck = Player.deal_to_player deck dealer in
  Player.print_player_hand player;
  print_string
    "Would you like to check or 4x or 3x your bet? Enter [4x], [3x], or [check].\n\
     > ";
  let play = Chips.prompt_for_set_bet (Chips.create ante) (snd chips) 1 in
  if ante <> Chips.total (fst play) then
    play_river_round play deck player dealer ante blind trips
  else play_flop_round deck (snd chips) player dealer ante blind trips

and play_river_round play deck player dealer ante blind trips =
  let flop, deck = deal_flop deck in
  let turn, deck = deal_turn deck flop in
  let river, _ = deal_river deck turn in
  let () = print_known_hands (Player.get_hand player) river in
  Player.print_player_hand dealer;
  let player_eval = Player.evaluate_hand (Player.get_hand player) river in
  let () = print_endline ("You had a " ^ player_eval ^ "!") in
  let dealer_eval = Player.evaluate_hand (Player.get_hand dealer) river in
  let () = print_endline ("Dealer had a " ^ dealer_eval ^ "!") in
  let outcome =
    Player.compare_hands
      (Player.advanced_evaluate_hand (Player.get_hand player) river)
      (Player.advanced_evaluate_hand (Player.get_hand dealer) river)
  in
  let dealer_score = assign_score dealer_eval in
  let player_score = assign_score player_eval in
  do_outcome outcome player_score dealer_score play ante blind trips player
    dealer game_loop

and play_flop_round deck chips player dealer ante blind trips =
  let flop, deck = deal_flop deck in
  let () = print_endline "Below is your hand:" in
  let () = Deck.print_cards (Deck.to_deck (Player.get_hand player)) in
  let () = print_endline "Below is the flop:" in
  Deck.print_cards (Deck.to_deck flop);
  print_string "Would you like to check or bet 2x? Enter [check] or [2x].\n> ";
  let play =
    Chips.prompt_for_set_bet (Chips.create ante)
      (Chips.create (Chips.total chips))
      2
  in
  if ante <> Chips.total (fst play) then
    play_river_round_2 play deck flop player dealer ante blind trips
  else play_final_round deck flop chips player dealer ante blind trips

and play_river_round_2 play deck flop player dealer ante blind trips =
  let turn, deck = deal_turn deck flop in
  let river, _ = deal_river deck turn in
  let () = print_known_hands (Player.get_hand player) river in
  Player.print_player_hand dealer;
  let player_eval = Player.evaluate_hand (Player.get_hand player) river in
  let () = print_endline ("You had a " ^ player_eval ^ "!") in
  let dealer_eval = Player.evaluate_hand (Player.get_hand dealer) river in
  let () = print_endline ("Dealer had a " ^ dealer_eval ^ "!") in
  let dealer_score = assign_score dealer_eval in
  let player_score = assign_score player_eval in
  let outcome =
    Player.compare_hands
      (Player.advanced_evaluate_hand (Player.get_hand player) river)
      (Player.advanced_evaluate_hand (Player.get_hand dealer) river)
  in
  do_outcome outcome player_score dealer_score play ante blind trips player
    dealer game_loop

and play_final_round deck flop chips player dealer ante blind trips =
  let turn, deck = deal_turn deck flop in
  let river, _ = deal_river deck turn in
  let () = print_known_hands (Player.get_hand player) river in
  print_string "Would you like to fold or bet 1x? Enter [fold] or [1x].\n> ";
  let play =
    Chips.prompt_for_set_bet (Chips.create ante)
      (Chips.create (Chips.total chips))
      3
  in
  if ante = Chips.total (fst play) then
    play_river_check_round play deck turn player dealer ante blind trips
  else play_river_fold_round deck turn player dealer ante blind trips chips

and play_river_check_round play deck turn player dealer ante blind trips =
  let river, _ = deal_river deck turn in
  let () = print_known_hands (Player.get_hand player) river in
  Player.print_player_hand dealer;
  let player_eval = Player.evaluate_hand (Player.get_hand player) river in
  let () = print_endline ("You had a " ^ player_eval ^ "!") in
  let dealer_eval = Player.evaluate_hand (Player.get_hand dealer) river in
  let () = print_endline ("Dealer had a " ^ dealer_eval ^ "!") in
  let dealer_score = assign_score dealer_eval in
  let player_score = assign_score player_eval in
  let outcome =
    Player.compare_hands
      (Player.advanced_evaluate_hand (Player.get_hand player) river)
      (Player.advanced_evaluate_hand (Player.get_hand dealer) river)
  in
  do_outcome outcome player_score dealer_score play ante blind trips player
    dealer game_loop

and play_river_fold_round deck turn player dealer ante blind trips total =
  let river, _ = deal_river deck turn in
  let () = print_known_hands (Player.get_hand player) river in
  Player.print_player_hand dealer;
  let player_eval = Player.evaluate_hand (Player.get_hand player) river in
  let () = print_endline ("You had a " ^ player_eval ^ "!") in
  let dealer_eval = Player.evaluate_hand (Player.get_hand dealer) river in
  let () = print_endline ("Dealer had a " ^ dealer_eval ^ "!") in
  let dealer_score = assign_score dealer_eval in
  let player_score = assign_score player_eval in
  let chips =
    process_loss dealer_score player_score
      (Chips.create 0, total)
      (Chips.create ante) (Chips.create blind) trips
  in
  game_loop Deck.full_deck chips player dealer

and do_outcome outcome player_score dealer_score play ante blind trips player
    dealer game_loop =
  if outcome > 0 then
    let chips =
      process_win player_score play (Chips.create ante) (Chips.create blind)
        trips
    in
    game_loop Deck.full_deck chips player dealer
  else if outcome = 0 then
    let chips =
      process_push player_score play (Chips.create ante) (Chips.create blind)
        trips
    in
    game_loop Deck.full_deck chips player dealer
  else
    let chips =
      process_loss dealer_score player_score play (Chips.create ante)
        (Chips.create blind) trips
    in
    game_loop Deck.full_deck chips player dealer

let start_game deck =
  print_string "What is your name?\n> ";
  let name = read_line () in
  let player = Player.create_player name in
  let dealer = Player.create_player "Dealer" in
  let starting_chips = Chips.create 1000 in
  print_endline ("\nWelcome player: " ^ Player.get_name player);
  game_loop deck starting_chips player dealer
