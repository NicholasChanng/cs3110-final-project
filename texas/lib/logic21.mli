(* Game Logic for Blackjack *)

val num_aces : Deck.card list -> int
(** [num_aces hand] is the number of aces in [hand]. *)

val find_value : Deck.card list -> int
(** [find_value hand] is the value of [hand] in Blackjack. *)

val is_nat : Deck.card list -> bool
(** [is_nat hand] is whether [hand] is a Blackjack, meaning that [hand] is two
    cards that total to 21. *)

val is_bust : Deck.card list -> bool
(** [is_bust hand] is whether [hand] is a bust, meaning that [hand] has a value
    over 21. *)

val can_double_down : Deck.card list -> bool
(** [can_double_down hand] is whether [hand] allows for doubling down, meaning
    that [hand] is two cards that total to 9, 10, or 11. *)

val check_natural : Deck.card list -> Deck.card list -> string
(** [check_natural player_hand dealer_hand] is a string summarizing whether or
    not [player_hand] and [dealer_hand] are naturals. *)
