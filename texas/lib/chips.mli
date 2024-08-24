(* Chips.mli *)

type t
(** The type representing a player's chip stack. *)

(** The type representing what kind of bet a player is making. *)
type bet =
  | Trips
  | Ante
  | Four
  | Three
  | Two
  | One

val create : int -> t
(** [create amount] creates a chip stack with the given initial amount. *)

val bet : t -> int -> t
(** [bet chips amount] bets a certain amount of chips from the stack. Raises an
    exception if the bet amount exceeds the available chips. *)

val add : t -> int -> t
(** [add chips amount] adds a specified amount of chips to the stack. *)

val total : t -> int
(** [total chips] returns the total number of chips in the stack. *)

val prompt_for_betting : t -> t * t
(** [prompt_for_betting initial] returns a tuple of the form
    [(bet, remaining chips)] after prompting a player with [initial] chips. *)

val ultimate_prompt_for_betting : bet -> t -> bool -> t * t
(** [ultimate_prompt_for_betting bet_type chips is_optional] prompts a player to
    bet a certain amount of chips based on the [bet_type]. If [is_optional] is
    true, the function will check if betting chips is nonnegative instead of
    positive integer. *)

val prompt_for_set_bet : t -> t -> int -> t * t
(** [prompt_for_set_bet chips bet] prompts a player to bet a certain amount of
    chips or fold, check. *)

val blind_payout : t -> int -> t
(** [blind_payout chips amount] returns the chip stack after a blind payout. *)

val trips_payout : t -> int -> t
(** [trips_payout chips amount] returns the chip stack after a trips payout. *)
