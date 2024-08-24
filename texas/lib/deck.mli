(** The type representing the rank of a playing card, like [Two], [Jack], or
    [Ace]. *)
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

(** The type representing the suit of a playing card, like [Spades] or
    [Diamonds]. *)
type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type card
(** The type representing a playing card. *)

type t
(** The type representing a hand or deck of cards. *)

val compare_ranks : rank -> rank -> int
(** [compare_ranks rank1 rank2] is the comparison of [rank1] and [rank2]. If
    [rank1] is a higher value than [rank2] then a positive number is returned.
    If [rank1] is a lower value than [rank2] then a negative number is returned.
    If [rank1] and [rank2] are the same rank then 0 is returned. *)

val all_suits : suit list
(** [all_suits] is all of the suits of playing cards. *)

val all_ranks : rank list
(** [all_ranks] is all of the ranks of playing cards. *)

val string_of_rank : rank -> string
(** [string_of_rank rank] is the string representation of [rank]. *)

val full_deck : t
(** [full_deck] is an unshuffled standard 52-card deck of playing cards. *)

val shuffle : t -> t
(** [shuffle deck] is the shuffled version of [deck]. *)

val draw : t -> card * t
(** [draw deck] is the tuple [(top_card, new_deck)] where [top_card] is the
    topmost card and [new_deck] is the remaining cards of the deck. Requires:
    [deck] is not empty. *)

val print_card : card -> unit
(** [print_card card] prints the rank and suit of [card], for example
    [Two of Spades]. *)

val print_cards : t -> unit
(** [print_cards cards] prints a list of cards [cards] side by side, followed by
    their rank and suit. *)

val to_deck : card list -> t
(** [to_deck card_lst] converts [card_lst] to the representation type of a deck *)

val get_rank : card -> rank
(** [get_rank card] is the rank of [card]. *)

val get_suit : card -> suit
(** [get_suit card] is the suit of [card] *)

val make_card : rank -> suit -> card
(** [make_card rank suit] creates a card given a [rank] and [suit]. *)

val rank_to_int : rank -> int
(** [rank_to_int] converts [rank] to an int*)

val alternate_compare_ranks : rank -> rank -> int
(** [alternate_compare_ranks] compares ranks without providing the difference
    between the cards' ranks*)

val get_card_lines : card -> string list
(** [get_card_lines card] returns the lines of the card's string representation. *)

val merge_card_lines : string list list -> string list
(** [merge_card_lines card_lines_list] merges multiple card lines into a single
    list of strings for side-by-side display. *)
