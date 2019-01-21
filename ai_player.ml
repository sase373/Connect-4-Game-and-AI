 
#use "sig_game.ml";;
#use "sig_player.ml";;

(* DELETE THIS WHEN RUNNING (if not working )*)
#use "game.ml";;

module AIPlayer = functor (Game: GAME) ->
struct
  module PlayerGame = Connect4

(*I/P: a list of estimates of boards and the moves that produce them *)
(*O/P: the move paired to the highest estimate *)
let rec max_move : (float * PlayerGame.move) list -> (float * PlayerGame.move)
  = function lst -> match lst with
     [] -> failwith "cant take max of empty list"
   | (e, m)::[] -> (e, m)
   | (e, m)::(e2, m2)::tl -> if (e>e2) then max_move ((e,m)::tl)
                                       else max_move ((e2, m2)::tl);;

(*I/P: a list of estimates of boards and the moves that produce them *)
(*O/P: the move paired to the lowest estimate *)
let rec min_move : (float * PlayerGame.move) list -> (float * PlayerGame.move)
 = function lst -> match lst with
     [] -> failwith "cant take min of empty list"
   | (e, m)::[] -> (e, m)
   | (e, m)::(e2, m2)::tl -> if (e<e2) then min_move ((e,m)::tl)
                                       else min_move ((e2, m2)::tl);;

(*I/P: a list of estimates of each move *)
(*O/P: the highest estimate*)
let rec max_val : float list -> float = function lst -> match lst with
     [] -> failwith "cant take max of empty list"
   | (e)::[] -> (e)
   | (e)::(e2)::tl -> if (e>e2) then max_val (e::tl) else max_val (e2::tl) ;;

(*I/P: a list of estimates of each move *)
(*O/P: the lowest estimate*)
let rec min_val : float list -> float = function lst -> match lst with
    [] -> failwith "cant take min of empty list"
  | (e)::[] -> (e)
  | (e)::(e2)::tl -> if (e<e2) then min_val (e::tl) else min_val (e2::tl) ;;

(*I/P: a state *)
(*O/P:  a list of all the new states produced from all the possible new moves *)
let rec est_move_list : state -> float list = function s -> List.map
  (function m -> (PlayerGame.estimate_value (PlayerGame.next_state s m)))
    (PlayerGame.legal_moves s) ;;

(*I/P: a state and a number of levels*)
(*O/P: the highest or lowest (depending on extreme_val) estimate that
can be produced from a set of moves*)
let rec minimax : state * int -> float = function (s, l) ->
(*I/P: a player *)
(*O/P: the procedure max_val if the player is player one, min_val otherwise *)
let rec extreme_val : which_player -> (float list -> float) = function p ->
  if (p = P1) then max_val else min_val
  in
    (*I/P: a state *)
    (*O/P: a list of new states coming from the legal moves list *)
    let rec new_states2 : state -> state list = function s -> List.map
      (function m -> (PlayerGame.next_state s m)) (PlayerGame.legal_moves s)
    in
  match (s,l) with
  |((State(Ongoing(p), b))), 0 -> ((extreme_val p) (est_move_list s))
  |((State(Ongoing(p), b))), n -> (extreme_val p) (List.map (function s ->
    (minimax (s, (n - 1)))) (new_states2 s))
  |((State(Win(p), b))), _ -> if (p=P1) then (100000.) else (-100000.)
  |((State(Draw, b))), _ -> (0.) ;;

(*I/P: a state *)
(*O/P: the best/worst (depending on extreme_move) that a player can make*)
let next_move : state -> PlayerGame.move = function s ->
  (*I/P: a state*)
  (*O/P: the highest estimate move tuple*)
  let next_move_helper : state -> (float * PlayerGame.move) = function s ->
    (*I/P: a state*)
    (*O/P: a list of state move tuples*)
    let next_states : state -> (state * PlayerGame.move) list = function s ->
      List.map (fun m -> (next_state s m), m) (PlayerGame.legal_moves s)
  in
  (*I/P: a player *)
  (*O/P: the procedure max_moves if the player is player 1, min_moves otherwise *)
  let rec extreme_move
    :which_player -> (float * PlayerGame.move) list -> (float * PlayerGame.move)
      = function p -> if (p = P1) then max_move else min_move
        in
        match s with
        | State(Ongoing(p), b) -> ((extreme_move p) ((List.map
          (function (s, m) -> (minimax (s,3)), m)) (next_states s)))
        | State(Win(p), b) -> failwith "Should not happen"
        | State(Draw, b) -> failwith "Should not happen"
    in
 match (next_move_helper s) with (e,x) -> x ;;

  end;;

  (* #use "ai_player.ml" ;; *)

let prac_boarda = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 1; 0; 0; 0];
                   [0; 0; 1; 1; 2; 0; 0];
                   [1; 1; 2; 2; 2; 0; 0]];;

let prac_boardb = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 1; 1; 2; 0; 0];
                   [0; 2; 1; 2; 2; 1; 0];
                   [2; 1; 2; 1; 1; 1; 0]];;

let prac_boardc = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 1; 0; 0; 0; 0; 0];
                   [0; 2; 0; 1; 0; 0; 0];
                   [0; 2; 1; 1; 2; 0; 0];
                   [1; 1; 2; 2; 2; 1; 0]];;

let prac_boardd = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 2; 1; 2; 0; 0];
                   [0; 2; 1; 1; 2; 1; 0];
                   [1; 1; 2; 2; 2; 1; 0]];;

let prac_boarde = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 1; 0; 0; 0];
                   [0; 0; 1; 1; 2; 0; 0];
                   [0; 1; 2; 2; 2; 0; 0]];;

let prac_boardf = [[0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 0; 0; 0; 0];
                   [0; 0; 0; 1; 2; 0; 0];
                   [0; 1; 2; 2; 1; 0; 0]];;

(* pratice boards *)
let prac_board1 = [[0; 0; 0; 1; 0; 0; 0];
                   [0; 0; 0; 1; 0; 0; 2];
                   [0; 0; 0; 1; 0; 0; 1];
                   [0; 0; 0; 1; 0; 1; 1];
                   [2; 1; 2; 1; 2; 1; 1]];;
let prac_board2 = [[0; 0; 0; 1; 0; 0; 1];
                   [0; 0; 0; 1; 0; 0; 2];
                   [0; 0; 0; 1; 0; 0; 1];
                   [0; 0; 0; 1; 0; 1; 1];
                   [2; 1; 2; 1; 2; 1; 1]];;
let prac_board3 = [[2; 1; 2; 1; 0; 0; 0];
                   [2; 1; 2; 1; 0; 0; 2];
                   [1; 2; 1; 1; 0; 0; 1]];;

(* example board for checking functions *)
let prac_board4 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 0; 0; 0; 2];
          [0; 0; 0; 1; 0; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 1; 2; 1; 1]];;
let prac_board5 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 1; 1; 0; 2];
          [0; 0; 0; 1; 2; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 1; 2; 1; 1]];;
let prac_board6 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 0; 0; 0; 2];
          [0; 0; 0; 1; 0; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 2; 2; 2; 1]];;
let prac_board7 = [[1; 1; 1; 2; 2; 2; 1];
          [2; 1; 2; 2; 1; 2; 2];
          [2; 2; 1; 1; 1; 2; 1];
          [1; 1; 2; 2; 1; 1; 1];
          [2; 1; 2; 2; 2; 1; 1]];;
let prac_board8 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 0; 1; 0; 2];
          [0; 0; 0; 1; 0; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 1; 2; 1; 1]];;
let prac_board9 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 0; 1; 0; 2];
          [0; 0; 0; 2; 0; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 2; 2; 1; 1]];;
let prac_board10 = [[0; 0; 0; 0; 1; 0; 0];
          [1; 1; 0; 0; 1; 0; 2];
          [0; 1; 0; 2; 0; 2; 1];
          [0; 0; 1; 1; 2; 1; 1];
          [2; 1; 2; 1; 2; 1; 1]];;
let prac_board11 = [[0; 0; 0; 0; 1; 0; 0];
          [0; 1; 0; 0; 0; 0; 2];
          [0; 0; 0; 1; 0; 2; 1];
          [0; 0; 0; 1; 0; 1; 1];
          [2; 1; 2; 1; 2; 0; 1]];;

 module Exampleai = AIPlayer(Connect4) ;;


(* check_expects for max_move *)
check_error (function () -> Exampleai.max_move [])
  "cant take max of empty list" ;;
check_expect (Exampleai.max_move [(2., Move 1)]) (2., Move 1) ;;
check_expect (Exampleai.max_move [(2., Move 1); (4., Move 3)]) (4., Move 3) ;;
check_expect (Exampleai.max_move [(2., Move 1); (4., Move 3); (6., Move 4);
  (1., Move 5)]) (6., Move 4) ;;

(* check_expects for min_move *)
check_error (function () -> Exampleai.min_move [])
  "cant take min of empty list" ;;
check_expect (Exampleai.min_move [(2., Move 1)]) (2., Move 1) ;;
check_expect (Exampleai.min_move [(2., Move 1); (4., Move 3)]) (2., Move 1) ;;
check_expect (Exampleai.min_move [(2., Move 1); (4., Move 3);
  (6., Move 4); (1., Move 5)]) (1., Move 5) ;;

(* check_expects for max_val *)
check_error (function () -> Exampleai.max_val [])
  "cant take max of empty list" ;;
check_expect (Exampleai.max_val [1.]) 1. ;;
check_expect (Exampleai.max_val [8.; 2.]) 8. ;;
check_expect (Exampleai.max_val [3.; 2.; 4.;]) 4. ;;

(* check_expects for min_val *)
check_error (function () -> Exampleai.min_val [])
  "cant take min of empty list" ;;
check_expect (Exampleai.min_val [1.]) 1. ;;
check_expect (Exampleai.min_val [8.; 2.]) 2. ;;
check_expect (Exampleai.min_val [3.; 2.; 4.;]) 2. ;;

(* check_expects for est_move_list *)
check_expect (Exampleai.est_move_list(State(Ongoing(P1), initial_board)))
  [3.; 3.; 3.; 4.; 3.; 3.; 3.] ;;
check_expect (Exampleai.est_move_list(State(Ongoing(P1), prac_board1)))
  [10000000.; 10000000.; 10000000.; 10000000.; 10000000.; 10000000.];;
check_expect (Exampleai.est_move_list(State(Ongoing(P1), prac_board3)))
  [1001.; 1001.; 5.] ;;
check_expect (Exampleai.est_move_list(State(Ongoing(P1), prac_board4)))
  [21.; 19.; 22.; 10000000.; 20.; 19.] ;;
check_expect (Exampleai.est_move_list(State(Ongoing(P1), prac_board8)))
  [10000000.; 10000000.; 10000000.; 10000000.; 10000000.; 10000000.] ;;
check_expect (Exampleai.est_move_list(State(Ongoing(P1), prac_board11)))
  [1017.; 20.; 1018.; 10000000.; 20.; 19.] ;;


(* check_expects for minimax*)
check_expect(Exampleai.minimax((State(Ongoing(P1), initial_board)), 2)) 3.;;
check_expect(Exampleai.minimax((State(Ongoing(P2), prac_board1)), 0))
  (-10000000.);;
check_expect(Exampleai.minimax((State(Ongoing(P1), prac_boarda)), 2)) 10.;;
check_expect(Exampleai.minimax((State(Ongoing(P2), prac_boardb)), 3)) 100000.;;
check_expect(Exampleai.minimax((State(Ongoing(P2), prac_boardc)), 1)) 6.;;
check_expect(Exampleai.minimax((State(Ongoing(P1), prac_boardd)), 2)) 100000.;;
check_expect(Exampleai.minimax((State(Ongoing(P1), prac_boarde)), 5))  4.;;
check_expect(Exampleai.minimax((State(Ongoing(P2), prac_boardf)), 0)) (-4.);;

(* check_expects for next_move *)
check_expect(Exampleai.next_move (initial_state)) (Exampleai.PlayerGame.Move 5);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boarda))))
  (Exampleai.PlayerGame.Move 6);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boardb))))
  (Exampleai.PlayerGame.Move 3);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boardc))))
  (Exampleai.PlayerGame.Move 4);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boardd))))
  (Exampleai.PlayerGame.Move 5);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boarde))))
  (Exampleai.PlayerGame.Move 6);;
check_expect(Exampleai.next_move ((State(Ongoing(P1), prac_boardf))))
  (Exampleai.PlayerGame.Move 4);;

