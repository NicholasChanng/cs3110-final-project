open Yojson.Basic.Util

let rec stand player_total dealer_hand deck =
  let dealer_total = Logic21.find_value dealer_hand in
  if dealer_total > 21 then
    let () =
      print_endline
        ("The dealer has busted with " ^ string_of_int dealer_total ^ ".")
    in
    let () = Deck.print_cards (Deck.to_deck dealer_hand) in
    "player win"
  else if dealer_total >= 17 then
    let () =
      print_endline
        ("The dealer's hand (below) has the total " ^ string_of_int dealer_total
       ^ ".")
    in
    let () = Deck.print_cards (Deck.to_deck dealer_hand) in
    if dealer_total > player_total then "dealer win"
    else if dealer_total < player_total then "player win"
    else "stand-off"
  else
    let draw = Deck.draw deck in
    stand player_total (fst draw :: dealer_hand) (snd draw)

let rec prompt_turn () =
  let () = print_string "What would you like to do? (hit/stand)\n> " in
  let decision = read_line () in
  if decision = "hit" || decision = "stand" then decision
  else
    let () = print_endline "That's an invalid response." in
    prompt_turn ()

let rec take_turn (player_hand : Deck.card list) dealer_hand deck =
  let () = print_endline "Below is the dealer's face-up card:" in
  let () = Deck.print_card (fst (Deck.draw (Deck.to_deck dealer_hand))) in
  let () = print_endline "" in
  let () = print_endline "Your hand is below:" in
  let () = Deck.print_cards (Deck.to_deck player_hand) in
  let player_total = Logic21.find_value player_hand in
  if player_total > 21 then
    let () = print_endline "Bust! Round over!" in
    "dealer win"
  else
    let decision = prompt_turn () in
    if decision = "hit" then
      let draw = Deck.draw deck in
      take_turn (fst draw :: player_hand) dealer_hand (snd draw)
    else
      let () =
        print_endline ("Your hand total is " ^ string_of_int player_total)
      in
      stand player_total dealer_hand deck

let handle_result bet result bank =
  if result = "blackjack" then
    let () =
      print_endline "Because of your Blackjack, you earn 1.5 times your bet!"
    in
    let gains = int_of_float (float_of_int bet *. 1.5) in
    let () = print_endline ("You earned " ^ string_of_int gains ^ " chips.") in
    bet + gains + bank
  else if result = "player win" then
    let () = print_endline "Congratulations on winning this round!" in
    let () = print_endline ("You earned " ^ string_of_int bet ^ " chips.") in
    (bet * 2) + bank
  else if result = "dealer win" then
    let () = print_endline "You lost this round." in
    let () = print_endline ("You lost " ^ string_of_int bet ^ " chips.") in
    bank
  else
    let () = print_endline "There was a standoff." in
    let () =
      print_endline ("Your bet of " ^ string_of_int bet ^ " was returned.")
    in
    bet + bank

let reveal_cards player_hand dealer_hand =
  let () = print_endline "Below is the dealer's hand:" in
  let () = Deck.print_cards (Deck.to_deck dealer_hand) in
  let () = print_endline "" in
  let () = print_endline "Below is your hand:" in
  let () = Deck.print_cards (Deck.to_deck player_hand) in
  print_endline ""

let handle_naturals bet nats bank =
  match nats with
  | "dealer blackjack" ->
      let () = print_endline "The dealer had a Blackjack!" in
      handle_result bet "dealer win" bank
  | "blackjack" ->
      let () = print_endline "You had a Blackjack!" in
      handle_result bet "blackjack" bank
  | _ -> handle_result bet "standoff" bank

let rec check_double_down player_hand dealer_hand =
  let () = print_endline "Below is the dealer's face-up card:" in
  let () = Deck.print_card (fst (Deck.draw (Deck.to_deck dealer_hand))) in
  let () = print_endline "" in
  let () = print_endline "Your hand is below:" in
  let () = Deck.print_cards (Deck.to_deck player_hand) in
  let () =
    print_string
      "Your current hand total allows you to double down. Your bet will be \
       doubled, but you will only be dealt one more card. Would you like to \
       double down? (y/n)\n\
       > "
  in
  let input = read_line () in
  if input = "y" then true
  else if input = "n" then false
  else
    let () = print_endline "That's not a valid response. Let's try again." in
    check_double_down player_hand dealer_hand

let double_down player_hand dealer_hand bet deck bank =
  let new_bet = 2 * bet in
  let draw5 = Deck.draw deck in
  let player_hand = fst draw5 :: player_hand in
  let () = print_endline "Your hand is below:" in
  let () = Deck.print_cards (Deck.to_deck player_hand) in
  let player_total = Logic21.find_value player_hand in
  if Logic21.is_bust player_hand then
    let () = print_endline "Bust! Round over!" in
    let result = "dealer win" in
    handle_result new_bet result bank
  else
    let result = stand player_total dealer_hand deck in
    handle_result new_bet result bank

let play_round bet bank : int =
  let deck = Deck.shuffle Deck.full_deck in
  let draw1 = Deck.draw deck in
  let draw2 = Deck.draw (snd draw1) in
  let draw3 = Deck.draw (snd draw2) in
  let draw4 = Deck.draw (snd draw3) in
  let deck = snd draw4 in
  let player_hand = [ fst draw1; fst draw2 ] in
  let dealer_hand = [ fst draw3; fst draw4 ] in
  let nats = Logic21.check_natural player_hand dealer_hand in
  if nats = "NA" then
    if Logic21.can_double_down player_hand && bank > bet then
      let double_down_ask = check_double_down player_hand dealer_hand in
      if double_down_ask = true then
        double_down player_hand dealer_hand bet deck bank
      else
        let result = take_turn player_hand dealer_hand deck in
        handle_result bet result bank
    else
      let result = take_turn player_hand dealer_hand deck in
      handle_result bet result bank
  else
    let () = reveal_cards player_hand dealer_hand in
    handle_naturals bet nats bank

let rec round chips =
  if Chips.total chips <= 0 then print_endline "Game over! You are out of chips"
  else
    let () =
      print_endline ("You have " ^ string_of_int (Chips.total chips) ^ " chips.")
    in
    let decision = Chips.prompt_for_betting chips in
    let bet = fst decision in
    let new_total = play_round (Chips.total bet) (Chips.total (snd decision)) in
    round (Chips.create new_total)

let handle_init_chips input =
  match int_of_string_opt input with
  | None -> 0
  | Some x -> x

let rec init_chips () =
  let () = print_string "How many chips would you like to start with?\n> " in
  let input = read_line () in
  let initial_chips = handle_init_chips input in
  if initial_chips > 1 then initial_chips
  else
    let () = print_endline "Please enter a positive number of chips." in
    init_chips ()

let play () =
  let json = Yojson.Basic.from_file "data/printblackjack.json" in
  let rules = json |> member "rules" |> to_string in
  let goals = json |> member "goals" |> to_string in
  let preround = json |> member "preround" |> to_string in
  let finalrules = json |> member "finalrules" |> to_string in
  let () = print_endline rules in
  let () = print_endline goals in
  let () = print_endline preround in
  let () = print_endline finalrules in
  let () = print_endline "We hope you enjoy Blackjack!\n" in
  let initial_chips = init_chips () in
  round (Chips.create initial_chips)
