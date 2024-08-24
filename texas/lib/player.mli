(* Player.mli *)

type t
(** The type representing a player in the game, with a name and a hand of cards. *)

val create_player : string -> t
(** [create_player name] creates a new player with the given name and an empty
    hand. *)

val deal_to_player : Deck.t -> t -> Deck.t
(** [deal_to_player deck player] deals two cards from the deck to the player's
    hand and returns the updated deck. *)

val print_player_hand : t -> unit
(** [print_player_hand player] prints the player's hand to the standard output. *)

val evaluate_hand : Deck.card list -> Deck.card list -> string
(** [evaluate_hand hand community_cards] evaluates the combined strength of the
    player's hand and the community cards, returning a string description of the
    hand's rank. *)

val get_hand : t -> Deck.card list
(** [get_hand player] is the hand of [player]. *)

val get_name : t -> string
(** [get_name player] is the name of [player]. *)

val highest_rank : Deck.card list -> Deck.rank
(** [highest_rank] returns the highest rank of [cards]*)

val advanced_evaluate_hand :
  Deck.card list -> Deck.card list -> string * Deck.rank list
(** [advanced_evaluate_hand] evaluates the player's [hand] based on
    [community_cards] and considers kickers*)

val compare_hands : string * Deck.rank list -> string * Deck.rank list -> int
(**[compare_hands] compares the dealer's and player's hand*)

val sort_cards_by_rank : Deck.card list -> Deck.card list
(** [sort_cards_by_rank] sorts [cards] based on rank*)
