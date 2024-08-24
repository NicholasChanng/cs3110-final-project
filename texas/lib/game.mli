val deal_flop : Deck.t -> Deck.card list * Deck.t
(** [deal_flop deck] is the tuple [(flop, updated_deck)] where [flop] is
    [card1; card2; card3] of the game and [updated_deck] is the deck after
    dealing the 3 cards for the flop. *)

val deal_turn : Deck.t -> Deck.card list -> Deck.card list * Deck.t
(** [deal_flop deck flop] is the tuple [(turn, updated_deck)] where [turn] is
    [card1; card2; card3; card4] of the game and [updated_deck] is the deck
    after dealing another 1 more card in addition to the flop for the turn. *)

val deal_river : Deck.t -> Deck.card list -> Deck.card list * Deck.t
(** [deal_flop deck turn] is the tuple [(river, updated_deck)] where [river] is
    [card1; card2; card3; card4; card5] of the game and [updated_deck] is the
    deck after dealing another 1 more card in addition to the turn for the
    river. *)

val start_game : Deck.t -> unit
(** [start_game] is the starting of the game where a player with the name given
    in the terminal is created. *)
