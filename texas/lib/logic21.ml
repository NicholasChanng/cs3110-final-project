let value card =
  match Deck.get_rank card with
  | Ace -> 11
  | King -> 10
  | Queen -> 10
  | Jack -> 10
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2

let rec aceless_hand_value hand =
  match hand with
  | [] -> 0
  | h :: t -> value h + aceless_hand_value t

let rec sort_hand hand acc ace_acc =
  match hand with
  | [] -> (acc, ace_acc)
  | h :: t ->
      if Deck.get_rank h = Ace then sort_hand t acc (h :: ace_acc)
      else sort_hand t (h :: acc) ace_acc

let rec num_aces hand =
  match hand with
  | [] -> 0
  | h :: t -> if Deck.get_rank h = Ace then 1 + num_aces t else num_aces t

let rec add_ace_value acc aces =
  match aces with
  | 0 -> acc
  | _ ->
      if acc + 11 > 21 then add_ace_value (acc + 1) (aces - 1)
      else add_ace_value (acc + 11) (aces - 1)

let find_value hand =
  let sorted = sort_hand hand [] [] in
  let aceless_value = aceless_hand_value (fst sorted) in
  let aces = num_aces hand in
  add_ace_value aceless_value aces

let is_nat hand =
  let value = find_value hand in
  let size = List.length hand in
  size = 2 && value = 21

let is_bust hand =
  let value = find_value hand in
  value > 21

let can_double_down hand =
  let value = find_value hand in
  let size = List.length hand in
  size = 2 && (value = 9 || value = 10 || value = 11)

let check_natural player_hand dealer_hand =
  match (is_nat player_hand, is_nat dealer_hand) with
  | true, true -> "standoff"
  | true, false -> "blackjack"
  | false, true -> "dealer blackjack"
  | _ -> "NA"
