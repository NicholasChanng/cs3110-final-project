open OUnit2
open! Texas.Player
open! Texas.Game
open! Texas.Chips
open! Texas.Deck
open! Texas.Blackjack
open! Texas.Logic21

(* Define one card for each combination of rank and suit *)

let ace_of_spades = make_card Ace Spades
let king_of_spades = make_card King Spades
let queen_of_spades = make_card Queen Spades
let jack_of_spades = make_card Jack Spades
let ten_of_spades = make_card Ten Spades
let nine_of_spades = make_card Nine Spades
let eight_of_spades = make_card Eight Spades
let seven_of_spades = make_card Seven Spades
let six_of_spades = make_card Six Spades
let five_of_spades = make_card Five Spades
let four_of_spades = make_card Four Spades
let three_of_spades = make_card Three Spades
let two_of_spades = make_card Two Spades
let ace_of_hearts = make_card Ace Hearts
let king_of_hearts = make_card King Hearts
let queen_of_hearts = make_card Queen Hearts
let jack_of_hearts = make_card Jack Hearts
let ten_of_hearts = make_card Ten Hearts
let nine_of_hearts = make_card Nine Hearts
let eight_of_hearts = make_card Eight Hearts
let seven_of_hearts = make_card Seven Hearts
let six_of_hearts = make_card Six Hearts
let five_of_hearts = make_card Five Hearts
let four_of_hearts = make_card Four Hearts
let three_of_hearts = make_card Three Hearts
let two_of_hearts = make_card Two Hearts
let ace_of_clubs = make_card Ace Clubs
let king_of_clubs = make_card King Clubs
let queen_of_clubs = make_card Queen Clubs
let jack_of_clubs = make_card Jack Clubs
let ten_of_clubs = make_card Ten Clubs
let nine_of_clubs = make_card Nine Clubs
let eight_of_clubs = make_card Eight Clubs
let seven_of_clubs = make_card Seven Clubs
let six_of_clubs = make_card Six Clubs
let five_of_clubs = make_card Five Clubs
let four_of_clubs = make_card Four Clubs
let three_of_clubs = make_card Three Clubs
let two_of_clubs = make_card Two Clubs
let ace_of_diamonds = make_card Ace Diamonds
let king_of_diamonds = make_card King Diamonds
let queen_of_diamonds = make_card Queen Diamonds
let jack_of_diamonds = make_card Jack Diamonds
let ten_of_diamonds = make_card Ten Diamonds
let nine_of_diamonds = make_card Nine Diamonds
let eight_of_diamonds = make_card Eight Diamonds
let seven_of_diamonds = make_card Seven Diamonds
let six_of_diamonds = make_card Six Diamonds
let five_of_diamonds = make_card Five Diamonds
let four_of_diamonds = make_card Four Diamonds
let three_of_diamonds = make_card Three Diamonds
let two_of_diamonds = make_card Two Diamonds

let deck =
  [
    ace_of_spades;
    king_of_spades;
    queen_of_spades;
    jack_of_spades;
    ten_of_spades;
    nine_of_spades;
    eight_of_spades;
    seven_of_spades;
    six_of_spades;
    five_of_spades;
    four_of_spades;
    three_of_spades;
    two_of_spades;
    ace_of_hearts;
    king_of_hearts;
    queen_of_hearts;
    jack_of_hearts;
    ten_of_hearts;
    nine_of_hearts;
    eight_of_hearts;
    seven_of_hearts;
    six_of_hearts;
    five_of_hearts;
    four_of_hearts;
    three_of_hearts;
    two_of_hearts;
    ace_of_clubs;
    king_of_clubs;
    queen_of_clubs;
    jack_of_clubs;
    ten_of_clubs;
    nine_of_clubs;
    eight_of_clubs;
    seven_of_clubs;
    six_of_clubs;
    five_of_clubs;
    four_of_clubs;
    three_of_clubs;
    two_of_clubs;
    ace_of_diamonds;
    king_of_diamonds;
    queen_of_diamonds;
    jack_of_diamonds;
    ten_of_diamonds;
    nine_of_diamonds;
    eight_of_diamonds;
    seven_of_diamonds;
    six_of_diamonds;
    five_of_diamonds;
    four_of_diamonds;
    three_of_diamonds;
    two_of_diamonds;
  ]

let deck_tests =
  "Deck Suites"
  >::: [
         ("Ace vs King" >:: fun _ -> assert_equal 1 (compare_ranks Ace King));
         ("Two vs Jack" >:: fun _ -> assert_equal (-9) (compare_ranks Two Jack));
         ( "Queen vs Queen" >:: fun _ ->
           assert_equal 0 (compare_ranks Queen Queen) );
         ( "all_suits test" >:: fun _ ->
           assert_equal [ Spades; Hearts; Clubs; Diamonds ] all_suits );
         ( "all_ranks test" >:: fun _ ->
           assert_equal
             [
               Ace;
               King;
               Queen;
               Jack;
               Ten;
               Nine;
               Eight;
               Seven;
               Six;
               Five;
               Four;
               Three;
               Two;
             ]
             all_ranks );
         ("Ace" >:: fun _ -> assert_equal "Ace" (string_of_rank Ace));
         ("Five" >:: fun _ -> assert_equal "5" (string_of_rank Five));
         ("Full Deck" >:: fun _ -> assert_equal full_deck (to_deck deck));
         ( "Draw" >:: fun _ ->
           assert_equal ace_of_spades (fst (draw (to_deck deck))) );
         ( "Ace of Spades Rank" >:: fun _ ->
           assert_equal Ace (get_rank ace_of_spades) );
         ( "Seven of Hearts Rank" >:: fun _ ->
           assert_equal Seven (get_rank seven_of_hearts) );
         ( "Jack of Clubs Rank" >:: fun _ ->
           assert_equal Jack (get_rank jack_of_clubs) );
         ( "Ace of Spades Suit" >:: fun _ ->
           assert_equal Spades (get_suit ace_of_spades) );
         ( "Seven of Hearts Suit" >:: fun _ ->
           assert_equal Hearts (get_suit seven_of_hearts) );
         ( "Jack of Clubs Suit" >:: fun _ ->
           assert_equal Clubs (get_suit jack_of_clubs) );
         ( "King of Diamonds Rank" >:: fun _ ->
           assert_equal King (get_rank king_of_diamonds) );
         ( "Nine of Diamonds Rank" >:: fun _ ->
           assert_equal Nine (get_rank nine_of_diamonds) );
         ( "Three of Clubs Suit" >:: fun _ ->
           assert_equal Clubs (get_suit three_of_clubs) );
         ( "Ten of Spades Suit" >:: fun _ ->
           assert_equal Spades (get_suit ten_of_spades) );
         ("King" >:: fun _ -> assert_equal "King" (string_of_rank King));
         ("Nine" >:: fun _ -> assert_equal "9" (string_of_rank Nine));
         ( "Compare Ten vs Queen" >:: fun _ ->
           assert_equal (-2) (compare_ranks Ten Queen) );
         ( "Compare Seven vs Eight" >:: fun _ ->
           assert_equal (-1) (compare_ranks Seven Eight) );
         ( "Ace of Diamonds Suit" >:: fun _ ->
           assert_equal Diamonds (get_suit ace_of_diamonds) );
         ( "Two of Hearts Rank" >:: fun _ ->
           assert_equal Two (get_rank two_of_hearts) );
         ( "Four of Clubs Rank" >:: fun _ ->
           assert_equal Four (get_rank four_of_clubs) );
         ( "Queen of Spades Suit" >:: fun _ ->
           assert_equal Spades (get_suit queen_of_spades) );
         ( "Five of Diamonds Suit" >:: fun _ ->
           assert_equal Diamonds (get_suit five_of_diamonds) );
       ]

let _ = run_test_tt_main deck_tests

let tests =
  "Texas Suites"
  >::: [
         ( "royal_flush_1" >:: fun _ ->
           let hand = [ ace_of_diamonds; king_of_diamonds ] in
           let community_cards =
             [
               queen_of_diamonds;
               jack_of_diamonds;
               ten_of_diamonds;
               nine_of_diamonds;
               eight_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Royal Flush" );
         ( "royal_flush_2" >:: fun _ ->
           let hand = [ ace_of_spades; king_of_spades ] in
           let community_cards =
             [
               queen_of_spades;
               jack_of_spades;
               ten_of_spades;
               nine_of_spades;
               eight_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Royal Flush" );
         ( "straight_flush_1" >:: fun _ ->
           let hand = [ nine_of_clubs; eight_of_clubs ] in
           let community_cards =
             [
               seven_of_clubs;
               six_of_clubs;
               five_of_clubs;
               four_of_clubs;
               three_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight Flush" );
         ( "straight_flush_2" >:: fun _ ->
           let hand = [ eight_of_spades; seven_of_spades ] in
           let community_cards =
             [
               six_of_spades;
               five_of_spades;
               four_of_spades;
               three_of_spades;
               two_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight Flush" );
         ( "four_of_a_kind_1" >:: fun _ ->
           let hand = [ ace_of_spades; ace_of_clubs ] in
           let community_cards =
             [
               ace_of_hearts;
               ace_of_diamonds;
               king_of_diamonds;
               king_of_hearts;
               king_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Four of a Kind" );
         ( "four_of_a_kind_2" >:: fun _ ->
           let hand = [ king_of_hearts; king_of_spades ] in
           let community_cards =
             [
               king_of_clubs;
               king_of_diamonds;
               queen_of_spades;
               queen_of_clubs;
               queen_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Four of a Kind" );
         ( "full_house_1" >:: fun _ ->
           let hand = [ king_of_hearts; king_of_spades ] in
           let community_cards =
             [
               king_of_clubs;
               jack_of_hearts;
               jack_of_diamonds;
               queen_of_spades;
               queen_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Full House" );
         ( "full_house_2" >:: fun _ ->
           let hand = [ ace_of_hearts; ace_of_spades ] in
           let community_cards =
             [
               king_of_clubs;
               king_of_diamonds;
               ace_of_diamonds;
               queen_of_spades;
               queen_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Full House" );
         ( "flush_1" >:: fun _ ->
           let hand = [ ace_of_hearts; ten_of_hearts ] in
           let community_cards =
             [
               eight_of_hearts;
               five_of_hearts;
               three_of_hearts;
               two_of_clubs;
               four_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Flush" );
         ( "flush_2" >:: fun _ ->
           let hand = [ nine_of_clubs; seven_of_clubs ] in
           let community_cards =
             [
               six_of_clubs;
               four_of_clubs;
               three_of_clubs;
               two_of_spades;
               ace_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Flush" );
         ( "straight_1" >:: fun _ ->
           let hand = [ nine_of_diamonds; eight_of_clubs ] in
           let community_cards =
             [
               seven_of_hearts;
               six_of_spades;
               five_of_diamonds;
               four_of_diamonds;
               three_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight" );
         ( "straight_2" >:: fun _ ->
           let hand = [ eight_of_spades; seven_of_hearts ] in
           let community_cards =
             [
               six_of_clubs;
               five_of_hearts;
               four_of_hearts;
               three_of_clubs;
               two_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight" );
         ( "three_of_a_kind_1" >:: fun _ ->
           let hand = [ queen_of_hearts; queen_of_spades ] in
           let community_cards =
             [
               queen_of_clubs;
               ten_of_hearts;
               eight_of_spades;
               seven_of_diamonds;
               two_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Three of a Kind"
         );
         ( "three_of_a_kind_2" >:: fun _ ->
           let hand = [ ten_of_spades; ten_of_clubs ] in
           let community_cards =
             [
               ten_of_hearts;
               seven_of_spades;
               five_of_hearts;
               two_of_diamonds;
               ace_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Three of a Kind"
         );
         ( "two_pair_1" >:: fun _ ->
           let hand = [ ace_of_hearts; ace_of_spades ] in
           let community_cards =
             [
               king_of_clubs;
               king_of_diamonds;
               jack_of_diamonds;
               ten_of_hearts;
               ten_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Two Pair" );
         ( "two_pair_2" >:: fun _ ->
           let hand = [ jack_of_clubs; jack_of_spades ] in
           let community_cards =
             [
               ten_of_clubs;
               ten_of_diamonds;
               seven_of_hearts;
               seven_of_spades;
               six_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Two Pair" );
         ( "one_pair_1" >:: fun _ ->
           let hand = [ ten_of_diamonds; ten_of_clubs ] in
           let community_cards =
             [
               seven_of_hearts;
               six_of_diamonds;
               three_of_diamonds;
               two_of_diamonds;
               five_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "One Pair" );
         ( "one_pair_2" >:: fun _ ->
           let hand = [ nine_of_clubs; nine_of_spades ] in
           let community_cards =
             [
               eight_of_hearts;
               seven_of_clubs;
               five_of_diamonds;
               four_of_hearts;
               three_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "One Pair" );
         ( "high_card_1" >:: fun _ ->
           let hand = [ ace_of_clubs; king_of_spades ] in
           let community_cards =
             [
               queen_of_hearts;
               jack_of_diamonds;
               nine_of_spades;
               eight_of_spades;
               seven_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "High Card: Ace" );
         ( "high_card_2" >:: fun _ ->
           let hand = [ eight_of_hearts; seven_of_clubs ] in
           let community_cards =
             [
               six_of_diamonds;
               jack_of_spades;
               four_of_hearts;
               king_of_hearts;
               three_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "High Card: King"
         );
         ( "pair_of_kings" >:: fun _ ->
           let hand = [ king_of_clubs; king_of_diamonds ] in
           let community_cards =
             [
               ten_of_diamonds;
               jack_of_spades;
               four_of_hearts;
               queen_of_clubs;
               three_of_diamonds;
             ]
           in
           assert_equal
             (advanced_evaluate_hand hand community_cards)
             ("One Pair", [ King; King; Queen; Jack; Ten ]) );
         ( "ace_vs_king" >:: fun _ ->
           let hand1 = [ two_of_clubs; ace_of_clubs ] in
           let hand2 = [ king_of_clubs; five_of_clubs ] in
           let community_cards =
             [
               ten_of_diamonds;
               jack_of_spades;
               four_of_hearts;
               queen_of_clubs;
               three_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "random_hand" >:: fun _ ->
           let hand1 = [ ace_of_hearts; two_of_hearts ] in
           let community_cards =
             [
               ten_of_clubs;
               queen_of_spades;
               eight_of_spades;
               two_of_clubs;
               seven_of_diamonds;
             ]
           in
           let outcome = advanced_evaluate_hand hand1 community_cards in
           assert_equal outcome ("One Pair", [ Two; Two; Ace; Queen; Ten ]) );
         ( "random_hand_comparison" >:: fun _ ->
           let hand1 = [ ace_of_diamonds; ten_of_spades ] in
           let hand2 = [ ace_of_hearts; two_of_hearts ] in
           let community_cards =
             [
               ten_of_clubs;
               queen_of_spades;
               eight_of_spades;
               two_of_clubs;
               seven_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_flushes" >:: fun _ ->
           let hand1 = [ seven_of_diamonds; two_of_diamonds ] in
           let hand2 = [ king_of_diamonds; three_of_diamonds ] in
           let community_cards =
             [
               four_of_diamonds;
               five_of_diamonds;
               eight_of_spades;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome (-1) );
         ( "compare_straights" >:: fun _ ->
           let hand1 = [ five_of_spades; six_of_clubs ] in
           let hand2 = [ ace_of_diamonds; two_of_clubs ] in
           let community_cards =
             [
               three_of_clubs;
               four_of_diamonds;
               five_of_diamonds;
               seven_of_spades;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_full_houses" >:: fun _ ->
           let hand1 = [ four_of_clubs; four_of_diamonds ] in
           let hand2 = [ three_of_hearts; three_of_diamonds ] in
           let community_cards =
             [
               nine_of_clubs;
               nine_of_diamonds;
               nine_of_hearts;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_three_of_kind" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; four_of_clubs ] in
           let hand2 = [ three_of_hearts; three_of_diamonds ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_pairs" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; ace_of_clubs ] in
           let hand2 = [ nine_of_diamonds; king_of_clubs ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_high_cards" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; ace_of_clubs ] in
           let hand2 = [ ten_of_clubs; ace_of_clubs ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               four_of_clubs;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome (-1) );
         ( "four_of_a_kind_3" >:: fun _ ->
           let hand = [ ten_of_hearts; ten_of_spades ] in
           let community_cards =
             [
               ten_of_clubs;
               ten_of_diamonds;
               nine_of_hearts;
               eight_of_clubs;
               seven_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Four of a Kind" );
         ( "four_of_a_kind_4" >:: fun _ ->
           let hand = [ queen_of_hearts; queen_of_spades ] in
           let community_cards =
             [
               queen_of_clubs;
               queen_of_diamonds;
               ace_of_hearts;
               king_of_clubs;
               jack_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Four of a Kind" );
         ( "full_house_3" >:: fun _ ->
           let hand = [ jack_of_clubs; jack_of_diamonds ] in
           let community_cards =
             [
               jack_of_hearts;
               ten_of_clubs;
               ten_of_diamonds;
               nine_of_hearts;
               eight_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Full House" );
         ( "full_house_4" >:: fun _ ->
           let hand = [ queen_of_clubs; queen_of_diamonds ] in
           let community_cards =
             [
               queen_of_hearts;
               nine_of_clubs;
               nine_of_spades;
               eight_of_clubs;
               seven_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Full House" );
         ( "flush_3" >:: fun _ ->
           let hand = [ king_of_hearts; nine_of_hearts ] in
           let community_cards =
             [
               seven_of_hearts;
               seven_of_hearts;
               six_of_hearts;
               five_of_clubs;
               two_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Flush" );
         ( "flush_4" >:: fun _ ->
           let hand = [ queen_of_spades; nine_of_spades ] in
           let community_cards =
             [
               eight_of_spades;
               six_of_spades;
               five_of_spades;
               three_of_spades;
               two_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Flush" );
         ( "straight_3" >:: fun _ ->
           let hand = [ jack_of_spades; ten_of_clubs ] in
           let community_cards =
             [
               nine_of_diamonds;
               eight_of_hearts;
               seven_of_spades;
               six_of_clubs;
               five_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight" );
         ( "straight_4" >:: fun _ ->
           let hand = [ six_of_spades; five_of_clubs ] in
           let community_cards =
             [
               four_of_diamonds;
               three_of_hearts;
               two_of_clubs;
               king_of_spades;
               two_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Straight" );
         ( "three_of_a_kind_3" >:: fun _ ->
           let hand = [ nine_of_diamonds; nine_of_hearts ] in
           let community_cards =
             [
               nine_of_clubs;
               eight_of_hearts;
               two_of_diamonds;
               six_of_spades;
               five_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Three of a Kind"
         );
         ( "three_of_a_kind_4" >:: fun _ ->
           let hand = [ seven_of_spades; seven_of_hearts ] in
           let community_cards =
             [
               seven_of_clubs;
               six_of_clubs;
               two_of_hearts;
               four_of_spades;
               three_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Three of a Kind"
         );
         ( "two_pair_3" >:: fun _ ->
           let hand = [ king_of_hearts; queen_of_hearts ] in
           let community_cards =
             [
               king_of_spades;
               queen_of_diamonds;
               ten_of_clubs;
               ten_of_hearts;
               nine_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Two Pair" );
         ( "two_pair_4" >:: fun _ ->
           let hand = [ ace_of_spades; ace_of_hearts ] in
           let community_cards =
             [
               king_of_clubs;
               king_of_diamonds;
               nine_of_clubs;
               eight_of_spades;
               seven_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "Two Pair" );
         ( "one_pair_3" >:: fun _ ->
           let hand = [ six_of_diamonds; six_of_clubs ] in
           let community_cards =
             [
               five_of_hearts;
               four_of_clubs;
               seven_of_clubs;
               two_of_clubs;
               ace_of_diamonds;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "One Pair" );
         ( "one_pair_4" >:: fun _ ->
           let hand = [ jack_of_clubs; jack_of_hearts ] in
           let community_cards =
             [
               nine_of_diamonds;
               eight_of_hearts;
               two_of_spades;
               six_of_clubs;
               five_of_hearts;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "One Pair" );
         ( "high_card_3" >:: fun _ ->
           let hand = [ queen_of_clubs; jack_of_hearts ] in
           let community_cards =
             [
               ten_of_clubs;
               two_of_diamonds;
               eight_of_hearts;
               seven_of_spades;
               six_of_clubs;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "High Card: Queen"
         );
         ( "high_card_4" >:: fun _ ->
           let hand = [ ace_of_hearts; ten_of_diamonds ] in
           let community_cards =
             [
               king_of_clubs;
               two_of_diamonds;
               jack_of_hearts;
               nine_of_clubs;
               eight_of_spades;
             ]
           in
           assert_equal (evaluate_hand hand community_cards) "High Card: Ace" );
         ( "compare_flushes_2" >:: fun _ ->
           let hand1 = [ seven_of_diamonds; two_of_diamonds ] in
           let hand2 = [ king_of_diamonds; three_of_diamonds ] in
           let community_cards =
             [
               four_of_diamonds;
               five_of_diamonds;
               eight_of_spades;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome (-1) );
         ( "compare_straights_2" >:: fun _ ->
           let hand1 = [ five_of_spades; six_of_clubs ] in
           let hand2 = [ ace_of_diamonds; two_of_clubs ] in
           let community_cards =
             [
               three_of_clubs;
               four_of_diamonds;
               five_of_diamonds;
               seven_of_spades;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_full_houses_2" >:: fun _ ->
           let hand1 = [ four_of_clubs; four_of_diamonds ] in
           let hand2 = [ three_of_hearts; three_of_diamonds ] in
           let community_cards =
             [
               nine_of_clubs;
               nine_of_diamonds;
               nine_of_hearts;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_three_of_kind_2" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; four_of_clubs ] in
           let hand2 = [ three_of_hearts; three_of_diamonds ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_pairs_2" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; ace_of_clubs ] in
           let hand2 = [ nine_of_diamonds; king_of_clubs ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               ten_of_diamonds;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome 1 );
         ( "compare_high_cards_2" >:: fun _ ->
           let hand1 = [ nine_of_diamonds; ace_of_clubs ] in
           let hand2 = [ ten_of_clubs; ace_of_clubs ] in
           let community_cards =
             [
               nine_of_clubs;
               three_of_clubs;
               queen_of_clubs;
               two_of_clubs;
               four_of_clubs;
             ]
           in
           let outcome =
             compare_hands
               (advanced_evaluate_hand hand1 community_cards)
               (advanced_evaluate_hand hand2 community_cards)
           in
           assert_equal outcome (-1) );
       ]

let _ = run_test_tt_main tests
let natural1 = [ ace_of_spades; king_of_clubs ]
let natural2 = [ ace_of_clubs; queen_of_clubs ]
let natural3 = [ ace_of_clubs; jack_of_clubs ]
let natural4 = [ ace_of_clubs; ten_of_clubs ]
let five = [ two_of_clubs; three_of_clubs ]
let six = [ three_of_clubs; three_of_clubs ]
let twenty = [ ten_of_diamonds; jack_of_spades ]
let ace_overflow = [ ace_of_clubs; ten_of_clubs; two_of_clubs ]
let no_overflow = [ nine_of_hearts; ace_of_spades ]
let bust = [ ten_of_diamonds; ten_of_spades; five_of_clubs ]
let two_aces = [ ace_of_clubs; ace_of_hearts ]
let triple_aces = [ ace_of_clubs; ace_of_hearts; ace_of_spades ]

let low_value =
  [
    two_of_clubs;
    three_of_diamonds;
    four_of_spades;
    five_of_hearts;
    six_of_clubs;
  ]

let mixed_hand =
  [ seven_of_clubs; seven_of_diamonds; five_of_hearts; ace_of_spades ]

let tests21 =
  "Blackjack Suite"
  >::: [
         ("nat1" >:: fun _ -> assert_equal (find_value natural1) 21);
         ("nat2" >:: fun _ -> assert_equal (find_value natural2) 21);
         ("nat3" >:: fun _ -> assert_equal (find_value natural3) 21);
         ("nat4" >:: fun _ -> assert_equal (find_value natural4) 21);
         ( "both nats" >:: fun _ ->
           assert_equal (check_natural natural1 natural4) "standoff" );
         ("13" >:: fun _ -> assert_equal (find_value ace_overflow) 13);
         ("no overflow" >:: fun _ -> assert_equal (find_value no_overflow) 20);
         ("five" >:: fun _ -> assert_equal (find_value five) 5);
         ("six" >:: fun _ -> assert_equal (find_value six) 6);
         ("twenty" >:: fun _ -> assert_equal (find_value twenty) 20);
         ( "blackjack" >:: fun _ ->
           assert_equal (check_natural natural1 ace_overflow) "blackjack" );
         ( "dealer blackjack" >:: fun _ ->
           assert_equal (check_natural ace_overflow natural1) "dealer blackjack"
         );
         ( "dealer blackjack" >:: fun _ ->
           assert_equal (check_natural natural2 natural1) "standoff" );
         ( "not nat" >:: fun _ ->
           assert_equal (check_natural ace_overflow ace_overflow) "NA" );
         ("1 ace" >:: fun _ -> assert_equal (num_aces ace_overflow) 1);
         ("bust" >:: fun _ -> assert_equal (find_value bust) 25);
         ("two aces" >:: fun _ -> assert_equal (find_value two_aces) 12);
         ("triple aces" >:: fun _ -> assert_equal (find_value triple_aces) 13);
         ("low value" >:: fun _ -> assert_equal (find_value low_value) 20);
         ("mixed hand" >:: fun _ -> assert_equal (find_value mixed_hand) 20);
         ( "blackjack vs bust" >:: fun _ ->
           assert_equal (check_natural natural1 bust) "blackjack" );
         ( "bust vs blackjack" >:: fun _ ->
           assert_equal (check_natural bust natural1) "dealer blackjack" );
         ( "two aces vs blackjack" >:: fun _ ->
           assert_equal (check_natural two_aces natural1) "dealer blackjack" );
         ( "low value vs bust" >:: fun _ ->
           assert_equal (check_natural low_value bust) "NA" );
         ( "mixed hand vs natural" >:: fun _ ->
           assert_equal (check_natural mixed_hand natural3) "dealer blackjack"
         );
         ("num_aces two aces" >:: fun _ -> assert_equal (num_aces two_aces) 2);
         ( "num_aces triple aces" >:: fun _ ->
           assert_equal (num_aces triple_aces) 3 );
       ]

let _ = run_test_tt_main tests21
