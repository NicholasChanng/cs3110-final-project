type t = {
  name : string;
  mutable hand : Deck.card list;
}
(** Abstraction function: the record [{name; hand}] represents the player with
    name [name] and hand [hand]. Representation invariant: [hand] contains no
    duplicates. *)

let create_player name = { name; hand = [] }

let deal_to_player deck player =
  let card1, deck' = Deck.draw deck in
  let card2, deck'' = Deck.draw deck' in
  player.hand <- [ card1; card2 ];
  deck''

let print_player_hand player =
  print_endline (player.name ^ "'s hand:");
  Deck.print_cards (Deck.to_deck player.hand)

let get_hand player = player.hand
let get_name player = player.name

let sort_cards_by_rank cards =
  List.sort
    (fun card1 card2 ->
      Deck.compare_ranks (Deck.get_rank card1) (Deck.get_rank card2))
    cards

let highest_rank cards =
  let sorted = sort_cards_by_rank cards in
  Deck.get_rank (List.hd (List.rev sorted))

let rec take n lst =
  if n <= 0 then []
  else
    match lst with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs

(** [is_flush_help] is a helper function for [is_flush]*)
let is_flush_help cards suit =
  List.fold_left
    (fun acc card -> if Deck.get_suit card = suit then acc + 1 else acc)
    0 cards

(** [advanced_is_flush] returns if [cards] are a flush or not as is used by the
    [advanced_evaluate_hand] function*)
let advanced_is_flush cards =
  let suit_counts =
    List.fold_left
      (fun acc card ->
        let suit = Deck.get_suit card in
        let count = try List.assoc suit acc with Not_found -> 0 in
        (suit, count + 1) :: List.remove_assoc suit acc)
      [] cards
  in
  match List.find_opt (fun (_, count) -> count >= 5) suit_counts with
  | Some (suit, _) -> (true, suit)
  | None -> (false, Deck.Spades (* Default value *))

(** is_straight returns if [cards] are a straight or not*)
let is_straight cards =
  let sorted = sort_cards_by_rank cards in
  let sorted_with_ace_as_one =
    List.map
      (fun card ->
        if Deck.get_rank card = Deck.Ace then
          Deck.make_card Deck.Two (Deck.get_suit card)
        else card)
      sorted
  in

  let rec aux ranks counter =
    match ranks with
    | [] -> counter >= 5
    | [ _ ] -> counter >= 5
    | x :: y :: t ->
        if Deck.compare_ranks (Deck.get_rank x) (Deck.get_rank y) = -1 then
          aux (y :: t) (counter + 1)
        else if Deck.compare_ranks (Deck.get_rank x) (Deck.get_rank y) = 0 then
          aux (y :: t) counter
        else aux (y :: t) 1
  in

  let rec check_all_combinations cards =
    match cards with
    | [] -> false
    | _ :: _ when List.length cards < 5 -> false
    | _ ->
        let first_five = take 5 cards in
        let remaining = List.tl cards in
        if aux (sort_cards_by_rank first_five) 1 then true
        else check_all_combinations remaining
  in

  check_all_combinations sorted || check_all_combinations sorted_with_ace_as_one

(** [count_ranks] returns an association list containing the ranks in [cards]
    and their counts*)
let count_ranks cards =
  List.fold_left
    (fun acc card ->
      let rank = Deck.get_rank card in
      let count = try List.assoc rank acc with Not_found -> 0 in
      (rank, count + 1) :: List.remove_assoc rank acc)
    [] cards

(** [is_flush] returns if a [cards] are a flush or not*)
let is_flush cards =
  let hearts = is_flush_help cards Deck.Hearts in
  let diamonds = is_flush_help cards Deck.Diamonds in
  let spades = is_flush_help cards Deck.Spades in
  let clubs = is_flush_help cards Deck.Clubs in
  hearts >= 5 || diamonds >= 5 || spades >= 5 || clubs >= 5

let all_cards hand community_cards = hand @ community_cards

let evaluate_flush_and_straight all_cards =
  if is_flush all_cards && is_straight all_cards then
    if Deck.string_of_rank (highest_rank all_cards) = "Ace" then "Royal Flush"
    else "Straight Flush"
  else ""

let evaluate_four_of_a_kind rank_counts =
  if List.exists (fun (_, count) -> count = 4) rank_counts then "Four of a Kind"
  else ""

let evaluate_full_house rank_counts =
  if
    List.exists (fun (_, count) -> count = 3) rank_counts
    && List.exists (fun (_, count) -> count = 2) rank_counts
  then "Full House"
  else ""

let evaluate_three_of_a_kind rank_counts =
  if List.exists (fun (_, count) -> count = 3) rank_counts then
    "Three of a Kind"
  else ""

let evaluate_pairs rank_counts =
  let pairs = List.filter (fun (_, count) -> count = 2) rank_counts in
  if List.length pairs >= 2 then "Two Pair"
  else if List.length pairs = 1 then "One Pair"
  else ""

let evaluate_high_card all_cards =
  "High Card: " ^ Deck.string_of_rank (highest_rank all_cards)

let evaluate_hand hand community_cards =
  let all_cards = all_cards hand community_cards in
  let rank_counts = count_ranks all_cards in
  let result =
    ( ( ( ( ( evaluate_flush_and_straight all_cards |> fun r ->
              if r = "" then evaluate_four_of_a_kind rank_counts else r )
          |> fun r -> if r = "" then evaluate_full_house rank_counts else r )
        |> fun r ->
          if r = "" then if is_flush all_cards then "Flush" else "" else r )
      |> fun r ->
        if r = "" then if is_straight all_cards then "Straight" else "" else r
      )
    |> fun r -> if r = "" then evaluate_three_of_a_kind rank_counts else r )
    |> fun r -> if r = "" then evaluate_pairs rank_counts else r
  in
  if result = "" then evaluate_high_card all_cards else result

(* Helper method to find ranks with specific counts *)
let find_rank_with_count count rank_counts =
  List.find (fun (_, c) -> c = count) rank_counts |> fst

(* Helper method to filter cards by rank *)
let filter_cards_by_rank rank cards =
  List.filter (fun card -> Deck.get_rank card = rank) cards

(* Helper method to filter cards not by rank *)
let filter_cards_not_by_rank rank cards =
  List.filter (fun card -> Deck.get_rank card <> rank) cards

(* Helper method to handle straight flush or royal flush *)
let handle_straight_flush all_cards =
  let high_card = highest_rank all_cards in
  if high_card = Deck.Ace then ("Royal Flush", []) else ("Straight Flush", [])

(* Helper method to handle four of a kind *)
let handle_four_of_a_kind rank_counts sorted_all_cards =
  let four_of_a_kind_rank = find_rank_with_count 4 rank_counts in
  let primary_cards =
    filter_cards_by_rank four_of_a_kind_rank sorted_all_cards
  in
  let remaining_cards =
    filter_cards_not_by_rank four_of_a_kind_rank sorted_all_cards
  in
  let kickers = take 1 remaining_cards in
  ( "Four of a Kind",
    List.map Deck.get_rank primary_cards
    @ List.rev (List.sort Deck.compare_ranks (List.map Deck.get_rank kickers))
  )

(* Helper method to handle full house *)
let handle_full_house rank_counts sorted_all_cards =
  let three_of_a_kind_rank = find_rank_with_count 3 rank_counts in
  let pair_rank = find_rank_with_count 2 rank_counts in
  let primary_cards =
    List.filter
      (fun card ->
        Deck.get_rank card = three_of_a_kind_rank
        || Deck.get_rank card = pair_rank)
      sorted_all_cards
  in
  ("Full House", List.map Deck.get_rank primary_cards)

(* Helper method to handle flush *)
let handle_flush flush_suit sorted_all_cards =
  let flush_cards =
    List.filter (fun card -> Deck.get_suit card = flush_suit) sorted_all_cards
  in
  let kickers = take 5 flush_cards in
  ("Flush", List.map Deck.get_rank kickers)

(* Helper method to handle straight *)
let handle_straight rank_counts sorted_all_cards =
  let straight_cards =
    List.filter
      (fun card ->
        let rank = Deck.get_rank card in
        List.exists
          (fun r -> Deck.rank_to_int r - Deck.rank_to_int rank <= 4)
          (List.map fst rank_counts))
      sorted_all_cards
  in
  let kickers = take 5 straight_cards in
  ("Straight", List.map Deck.get_rank kickers)

(* Helper method to handle three of a kind *)
let handle_three_of_a_kind rank_counts sorted_all_cards =
  let three_of_a_kind_rank = find_rank_with_count 3 rank_counts in
  let primary_cards =
    filter_cards_by_rank three_of_a_kind_rank sorted_all_cards
  in
  let remaining_cards =
    filter_cards_not_by_rank three_of_a_kind_rank sorted_all_cards
  in
  let kickers = take 2 remaining_cards in
  ( "Three of a Kind",
    List.map Deck.get_rank primary_cards
    @ List.rev (List.sort Deck.compare_ranks (List.map Deck.get_rank kickers))
  )

(* Helper method to handle two pair *)
let handle_two_pair pairs sorted_all_cards =
  let pair1 = List.nth pairs 0 |> fst in
  let pair2 = List.nth pairs 1 |> fst in
  let primary_cards =
    List.filter
      (fun card -> Deck.get_rank card = pair1 || Deck.get_rank card = pair2)
      sorted_all_cards
  in
  let remaining_cards =
    List.filter
      (fun card -> not (List.mem (Deck.get_rank card) [ pair1; pair2 ]))
      sorted_all_cards
  in
  let kickers = take 1 remaining_cards in
  ( "Two Pair",
    List.map Deck.get_rank primary_cards
    @ List.rev (List.sort Deck.compare_ranks (List.map Deck.get_rank kickers))
  )

(* Helper method to handle one pair *)
let handle_one_pair pairs sorted_all_cards =
  let pair = List.nth pairs 0 |> fst in
  let primary_cards = filter_cards_by_rank pair sorted_all_cards in
  let remaining_cards = filter_cards_not_by_rank pair sorted_all_cards in
  let kickers = take 3 remaining_cards in
  ( "One Pair",
    List.map Deck.get_rank primary_cards
    @ List.rev (List.sort Deck.compare_ranks (List.map Deck.get_rank kickers))
  )

(* Helper method to handle high card *)
let handle_high_card sorted_all_cards =
  let primary_card = List.hd sorted_all_cards in
  let kickers = take 4 (List.tl sorted_all_cards) in
  ( "High Card: " ^ Deck.string_of_rank (Deck.get_rank primary_card),
    Deck.get_rank primary_card
    :: List.sort Deck.compare_ranks (List.map Deck.get_rank kickers) )

(* Main advanced_evaluate_hand function *)
let advanced_evaluate_hand hand community_cards =
  let all_cards = hand @ community_cards in
  let sorted_all_cards = List.rev (sort_cards_by_rank all_cards) in
  let rank_counts = count_ranks all_cards in
  let is_flush, flush_suit = advanced_is_flush all_cards in
  let is_straight = is_straight all_cards in
  if is_flush && is_straight then handle_straight_flush all_cards
  else if List.exists (fun (_, count) -> count = 4) rank_counts then
    handle_four_of_a_kind rank_counts sorted_all_cards
  else if
    List.exists (fun (_, count) -> count = 3) rank_counts
    && List.exists (fun (_, count) -> count = 2) rank_counts
  then handle_full_house rank_counts sorted_all_cards
  else if is_flush then handle_flush flush_suit sorted_all_cards
  else if is_straight then handle_straight rank_counts sorted_all_cards
  else if List.exists (fun (_, count) -> count = 3) rank_counts then
    handle_three_of_a_kind rank_counts sorted_all_cards
  else
    let pairs = List.filter (fun (_, count) -> count = 2) rank_counts in
    if List.length pairs >= 2 then handle_two_pair pairs sorted_all_cards
    else if List.length pairs = 1 then handle_one_pair pairs sorted_all_cards
    else handle_high_card sorted_all_cards

(** [hand_ranking] returns the rank of hand *)
let hand_ranking = function
  | "Royal Flush" -> 10
  | "Straight Flush" -> 9
  | "Four of a Kind" -> 8
  | "Full House" -> 7
  | "Flush" -> 6
  | "Straight" -> 5
  | "Three of a Kind" -> 4
  | "Two Pair" -> 3
  | "One Pair" -> 2
  | hand when String.sub hand 0 9 = "High Card" -> 1
  | _ -> 0

(** [compare_kickers] compares the kickers of two hands*)
let rec compare_kickers kickers1 kickers2 =
  match (kickers1, kickers2) with
  | [], [] -> 0
  | k1 :: t1, k2 :: t2 ->
      let cmp = Deck.alternate_compare_ranks k1 k2 in
      if cmp <> 0 then cmp else compare_kickers t1 t2
  | _ -> failwith "Kickers lists have different lengths"

let compare_hands (player_eval, player_kickers) (dealer_eval, dealer_kickers) =
  let player_hand_rank = hand_ranking player_eval in
  let dealer_hand_rank = hand_ranking dealer_eval in
  if player_hand_rank > dealer_hand_rank then 1
  else if player_hand_rank < dealer_hand_rank then -1
  else compare_kickers player_kickers dealer_kickers
