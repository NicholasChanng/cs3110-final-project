(** @author Nicholas Channg, Mehdi Heydari, Jerry Ji, Ryan King, Gavin Zhou, in collaboration with ChatGPT. *)

open! Texas.Deck
open! Texas.Player
open! Texas.Game
open Yojson.Basic.Util

let texas_init () =
  let json = Yojson.Basic.from_file "data/print.json" in
  let welcome_message = json |> member "welcome_message" |> to_string in
  let game_description = json |> member "game_description" |> to_string in
  let () = print_endline welcome_message in
  let () = print_endline game_description in
  let () = print_string "Would you like to start the game? (Y/N)\n> " in
  let answer = read_line () in
  if String.lowercase_ascii answer = "y" then (
    print_endline "\nGreat for you to join us!\n";
    start_game full_deck)
  else print_endline "Hope you change your mind!"

let blackjack_init () =
  let () = print_endline "Welcome to Blackjack!" in
  Texas.Blackjack.play ()

let rec choose_game () =
  let () =
    print_string
      "What game would you like to play? We have Texas Hold'em and Blackjack. \
       Enter [texas] or [blackjack].\n\
       > "
  in
  let game = read_line () in
  if String.lowercase_ascii game = "texas" then texas_init ()
  else if String.lowercase_ascii game = "blackjack" then blackjack_init ()
  else
    let () = print_endline "That's not a valid choice." in
    choose_game ()

let _ =
  let () =
    print_endline "Welcome! Fullscreen terminal is highly recommended."
  in
  choose_game ()
