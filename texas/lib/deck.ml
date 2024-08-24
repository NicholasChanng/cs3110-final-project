let () = Random.self_init ()

type rank =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two

type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type card = rank * suit

type t = card list
(** Abstraction function: the list [[a1; ...; an]] represents the hand or deck
    of cards [a1, ..., an]. The empty list [[]] represents the empty hand.
    Representation invariant: the list contains no duplicates. *)

let rank_to_int rank =
  match rank with
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2

let compare_ranks rank1 rank2 = rank_to_int rank1 - rank_to_int rank2

let alternate_compare_ranks rank1 rank2 =
  Int.compare (rank_to_int rank1) (rank_to_int rank2)

let all_ranks =
  [
    Ace; King; Queen; Jack; Ten; Nine; Eight; Seven; Six; Five; Four; Three; Two;
  ]

let all_suits = [ Spades; Hearts; Clubs; Diamonds ]

let full_deck =
  List.flatten
    (List.map
       (fun suit -> List.map (fun rank -> (rank, suit)) all_ranks)
       all_suits)

let shuffle (deck : t) : t =
  let arr = Array.of_list deck in
  let length = Array.length arr in
  for i = length - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let draw (deck : t) : card * t =
  match deck with
  | [] -> failwith "Empty Deck"
  | h :: t -> (h, t)

let string_of_rank rank =
  match rank with
  | Ace -> "Ace"
  | King -> "King"
  | Queen -> "Queen"
  | Jack -> "Jack"
  | Ten -> "⒑"
  | Nine -> "9"
  | Eight -> "8"
  | Seven -> "7"
  | Six -> "6"
  | Five -> "5"
  | Four -> "4"
  | Three -> "3"
  | Two -> "2"

let string_of_rank_aux rank =
  match rank with
  | Ace -> "A"
  | King -> "K"
  | Queen -> "Q"
  | Jack -> "J"
  | Ten -> "⒑"
  | Nine -> "9"
  | Eight -> "8"
  | Seven -> "7"
  | Six -> "6"
  | Five -> "5"
  | Four -> "4"
  | Three -> "3"
  | Two -> "2"

let string_of_suit suit =
  match suit with
  | Spades -> "Spades"
  | Hearts -> "Hearts"
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"

let string_of_suit_symbol suit =
  match suit with
  | Spades -> "♤"
  | Hearts -> "♡"
  | Clubs -> "♧"
  | Diamonds -> "♢"

let print_card card =
  print_endline "---------";
  print_endline "|       |";
  print_endline ("|     " ^ string_of_rank_aux (fst card) ^ " |");
  print_endline "|       |";
  print_endline ("|   " ^ string_of_suit_symbol (snd card) ^ "   |");
  print_endline "|       |";
  print_endline ("| " ^ string_of_rank_aux (fst card) ^ "     |");
  print_endline "|       |";
  print_endline "---------";
  print_endline (string_of_rank (fst card) ^ " of " ^ string_of_suit (snd card))

let get_card_lines card =
  let rank_aux = string_of_rank_aux (fst card) in
  let suit_symbol = string_of_suit_symbol (snd card) in
  [
    "---------";
    "|       |";
    "|     " ^ rank_aux ^ " |";
    "|       |";
    "|   " ^ suit_symbol ^ "   |";
    "|       |";
    "| " ^ rank_aux ^ "     |";
    "|       |";
    "---------";
  ]

let rec merge_card_lines card_lines_list =
  match card_lines_list with
  | [] -> List.init 9 (fun _ -> "")
  | card_lines :: rest ->
      let rest_lines = merge_card_lines rest in
      List.map2 (fun cl rl -> cl ^ "  " ^ rl) card_lines rest_lines

let print_cards cards =
  let card_lines_list = List.map get_card_lines cards in
  let merged_lines = merge_card_lines card_lines_list in
  List.iter print_endline merged_lines;
  List.iter
    (fun card ->
      print_endline
        (string_of_rank (fst card) ^ " of " ^ string_of_suit (snd card)))
    cards;
  print_endline ""

let to_deck (cards : card list) : t = cards
let get_rank (card : card) : rank = fst card
let get_suit (card : card) : suit = snd card
let make_card (rank : rank) (suit : suit) : card = (rank, suit)
