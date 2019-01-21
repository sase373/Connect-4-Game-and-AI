#use "CS17setup.ml" ;;
#use "sig_game.ml";;


module Connect4  = 
struct
type which_player = P1 | P2;;

type status =
| Win of which_player
| Draw
| Ongoing of which_player;;

type matrix = int list list;;

type state = State of (status * matrix)

type move = Move of int

let rec other_player  = function player ->
  match player with
  P1 -> P2
  | P2 -> P1

  let rec string_of_player = function player ->
  if player = P1 then "Player 1" else "Player 2"

  (* I/P: an int list *)
(* O/P: a string verison of the list-- with each int converted to a string *) 
(* and appended to the next entry, ending with an end line symbol *)

let rec row_to_string: int list -> string = function r -> 
  match r with
  [] -> "\n"
  | a::b -> string_of_int a ^ " " ^ row_to_string b;;

(* check_expect (row_to_string [1; 3; 1; 2]) "1 3 1 2 \n";; *)

(* I/P: a matrix, or int list *)
(* O/P: a string representation of the matrix *)

let rec string_of_matrix: matrix -> string = function m -> 
  match m with
  [] -> ""
  | r1::r2 -> row_to_string r1 ^ string_of_matrix r2;;

(* I/P: a State consisting of player, and a connect 4 board, or int matrix *)
(* O/P: a string representation of the state *)

let rec string_of_state: state -> string = function State(s, b) -> 
match (s, b) with
  Win (p), _ -> string_of_player 
   (other_player p) ^ " loses! The final board is \n"^ string_of_matrix (b)
  | Draw, _ -> "Draw. The final board is \n"^ string_of_matrix (b)
  | Ongoing (p), x -> 
  "It is " ^ string_of_player p ^ "'s turn. The current board is \n"^ 
  string_of_matrix (b)


(* I/P: a move *)
(* O/P: a string representation that move*)

let rec string_of_move: move -> string = function Move(x)
     -> "Column " ^ (string_of_int x);;

(* defining the initial dimensions of the connect 4 matrix *)

  let initial_rows: int = 5
  let initial_cols: int = 7

(* I/P: an integer, n *)
(* O/P: a list, containing (n-1) empty entries of zeros*)


let rec make_list: int -> 'a list  = function n ->
  match n with
  0 -> []
  | _ -> 0::(make_list (n -1));;


 (* I/P: two integers, a,b *)
 (* O/P: a matrix, consisting of a rows and b columns of zeros*)


let rec make_matrix: (int * int) -> 'a list list = function (a,b) ->
  match a with
  0 -> []
  | _ -> (make_list b)::make_matrix ((a -1), b);;


(* creating the intial matrix from given # of rows and columns *)

let initial_board = make_matrix (initial_rows,initial_cols);;

(* creating the intial State from matrix and player one *)

let initial_state = (State (Ongoing(P1), initial_board));;


(* I/P: a connect 4 board, or int matrix *)
(* O/P: a list of all the columns in the board that are not full *)

let rec legal_moves: state -> move list = function State(s, (board)) -> 
  match s with
  Ongoing(p) -> 

      let rec legal_list = function (row, n) ->
      (match row with
      [] -> []
      | a::b -> if (a=0) then Move(n)::legal_list(b, (n+1)) 
                         else legal_list(b, (n+1)))
       
        in legal_list (List.hd board, 1)

  |Win(p) -> [] 
  |Draw -> [];;

(* I/P: a board (int matrix), a move (as a column #) and a counter r *)
(* O/P: the row number that a token would fall in given a connect4move *)

let rec find_row: (matrix * int * int) -> int = function (board, move, r) ->
  match board with
  [] -> r
  | r1::[] -> r
  | r1::r2 -> if ((List.nth (List.hd r2) (move - 1)) != 0) 
              then (match (List.nth r1 (move - 1)) with 
                     0 -> r
                     | _ -> failwith "column full" ) 
              else find_row (r2, move, (r+1));;

(* I/P: a player *)
(* O/P: a number indicating player 1 or 2 *)

let rec token = function player -> if (player = P1) then 1 else 2;;

(* I/P: a player, playa, a row, and an integer represnting a column move *)
(* O/P: the original row with players token inserted in correct colum *)

let rec insert: (which_player * int list * int) -> int list 
  = function (playa, row, column) ->
      match row with
      []-> []
      | a::b -> if column = 1 then (token playa)::b 
        else a::insert (playa, b, (column -1));;

(* I/P: a player, a board, and a move *)
(* O/P: the new board, after putting the players token into the column *)

let rec move_to_board: (which_player * matrix * move) -> matrix = 
  function (player, board, (Move(n))) ->

    let rec make_move = function (playa, board, row, column) ->
       match board with
       [] -> failwith "incorrect row for identified for move"
       | (a::b) -> if row = 1 then (insert (playa, a, column))::b
                   else a:: (make_move (playa, b, (row -1), column))

 in make_move (player, board, (find_row (board, n, 1)), n)


(* these are functions copied directly form Homwork 8 *)

let rec vert_flip: matrix -> matrix = function mat ->
match mat with
 [] -> failwith "A matrix cannot be 0 - dimensional."
 | _::_ -> List.map List.rev mat;;

let rec transpose:  matrix ->  matrix = function mat ->
match mat with
| [] | [] :: _ -> failwith "A matrix cannot be 0 - dimensional."
| ((hd1 :: []) :: tl) -> [List.flatten mat]
| (hd1 :: tl1) :: tl ->
List.map List.hd mat :: transpose (List.map List.tl mat);;

(* I/P a row, and tally indicating # tokens of same player in a row *)
(* O/P: Some, of player who has 4 tokens consecutve in row, None, otherwise *)

let rec horiz_win: (int list * int) -> string option = function (row, tally) ->
  match (row, tally) with
  ([], _) -> failwith "shouldnt get here anyway"
  | (a::[], n) -> None
  | (0::b, n) -> horiz_win (b, 0)
  | (a::b,2) -> if (a=(List.hd b)) 
                then Some ("win by player" ^ (string_of_int a) ^"") 
                else horiz_win (b, 0)
  | (a::b, n) -> if (a=(List.hd b)) 
                 then horiz_win (b, (n+1)) 
                 else horiz_win (b, 0);;

(* I/P: takes in a matrix and an int, acting as an index *)
(* O/P: a list of the entries of each index for each row *)

let rec diagonal: ( matrix * int) -> int list = function (board, nth) -> 
match (board, nth) with
  ([], _)-> []
  | (a::b, 0) -> (List.hd a)::[]
  | (a::b, n) -> if (n < (List.length a))
                 then (List.nth a n)::diagonal(b, (n-1))
                 else diagonal (b, (n-1)) ;;

(* I/P: takes in a matrix representing a connect 4 board *)
(* O/P: the # of diags needed to collect, beginning at the relevant index *)
(*(-5 accounting for repeat columns & 0 indexed list) confusing, can explain *)

let rec diag_number: matrix -> int = function board -> 
  if (((List.length (List.hd board)) > 3) && ((List.length board) > 3))
  then (((List.length (List.hd board)) - 5) + (List.length board)) else 0;;

(* I/P: a matrix and an int, the number of diagonals needed to check *)
(* O/P: Some, if one of the diagonal entry lists contains a win, None if not *)

let rec check_diagonals: (matrix * int) -> string option = 
  function (board, n) ->
     if n < 3 then None
     else (match (horiz_win ((diagonal (board, n)), 0)) with
          None -> check_diagonals (board, (n-1))
          |Some(x) -> Some(x));;


(* I/P a matrix, representing a connect 4 board *)
(* O/P: None if 0 rows have 4 consecutive elements, Some of player otherwise *)

let rec win:  matrix -> string option = function (board) ->
  match board with
  [] -> None
  | a::b -> (match (horiz_win (a, 0)) with
      None -> win b
      | Some (x) -> Some (x));;

(* I/P a matrix, representing a connect 4 board *)
(* O/P: Win(p) if matrix contains a vert, horiz, or diag win, None otherwise *)


let rec any_win:  matrix -> string option = function board -> 
  match (win board) with
  | Some(x) -> Some(x)
  | None -> (match (win (transpose board)) with
            Some (x) -> Some(x)
            | None -> (match (check_diagonals ((board, (diag_number board))))
             with
                        Some(x) -> Some(x)
                       | None -> check_diagonals 
                       ((vert_flip board), (diag_number board))));;


(* I/P: A state *)
(* O/P: A status - whether connect4 is won, theres a draw or still playing *)

let rec game_status: state -> status  = function State(status, board) -> 
  match status with
  Ongoing(p) -> (match (any_win board) with
            None -> if ((legal_moves (State(status, board))) = []) 
                    then Draw else Ongoing(p)
          | Some(x) -> Win(other_player p))
  | _ -> status ;;


(* I/P: current state, and move of current player if the game is still going *)
(* O/P: the sequential state, having carried out the move (if there was one) *)

let rec next_state = function State (s, board) -> function Move(m) ->
  match s with
  Win (_)  -> State (s, board)
  | Draw -> State (s, board)
  | Ongoing(player) -> (match (move_to_board (player, board, Move(m))) with
                        new_board -> State ((game_status 
                          (State (Ongoing(other_player player), new_board))),
                              new_board)) ;;

(* I/P: a string of a number *)
(* O/P: a move of that number *)

let rec move_of_string: string -> move = function s -> Move(int_of_string s);;

(* I/P: a row, a counter (tally), an int representing the player,
 and a "block" representing # of spaces since other player or a matrix edge*)
(* O/P: an int representing the potenital of a win for a player based on # 
of adjacent tokens and blanks, 0 if there is a block less than 4 slots away*)

let rec horiz_three: (int list * int * int * int) -> int = 
  function (row, tally, tok, block) -> 
    match (row, tally, tok, block) with
    (_, 4 , _, _) -> 1000
    |([], n, t, b) -> if (b<1) then n else 0
    | (a::a1, n, t, b) -> (match (a) with
          | 0 -> horiz_three (a1, (n), t, (b-1))
          | _ -> if (a = t) then (horiz_three (a1, (n + 1), t, (b-1))) 
                            else (if (b < 1) 
                               then (n + horiz_three (a1, 0, t, 4))
                               else horiz_three (a1, 0, t, 4)));;
(* I/P: a board and a player *)
(* O/P: an estimate count of all rows for a given player's potenital wins*)

 let rec count: (matrix * int) -> int = function (board, p) ->
 match board with
 [] -> 0
 | a::b -> horiz_three(a, 0, p, 4) + count(b, p);;

(* I/P: a board, an int of # diagonals relevant to connect4 win, and player *)
(* O/P: est. total count of player potenital wins for all right->left diags *)

let rec diag_count: (matrix * int * int) -> int = function (board, n, p) ->
  if (n < 3) then 0 
             else horiz_three ((diagonal (board, n)), 0, p, 4) 
               + diag_count (board, (n-1), p);;

(* I/P: a board, and a player *)
(* O/P: an esitmate total count for how the board appears for the player *)

 let rec estimate_value1: (matrix * int) -> int =  function (board, p) -> 
    (match (p) with
      t  -> (count (board, t)) +
            (count (transpose board, t)) +
            (diag_count (board, (diag_number board), t)) +
            ((diag_count ((vert_flip board, (diag_number board), t)))));;

(* I/P: a State *)
(* O/P: an estimate value of the state from player 1's perspecitve*)

let rec estimate_value =  function State(s, board) ->
  match (game_status (State(s, board))) with
  Draw -> 0.
  | Win(p) -> if (p = P1) then 10000000. else -10000000.
  | Ongoing(p) ->  float_of_int((estimate_value1(board, 1))
                             - (estimate_value1(board, 2)));;


;;

end ;;


open Connect4 ;;


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

(* check expects for other_player *)
check_expect (other_player P1) P2 ;;
check_expect (other_player P2) P1 ;;

(* check expects for string_of_player *)
check_expect (string_of_player P1) "Player 1" ;;
check_expect (string_of_player P2) "Player 2" ;;

(* check expect for row_to_string *)
check_expect (row_to_string []) "\n";;
check_expect (row_to_string [4]) "4 \n";;
check_expect (row_to_string [1; 3; 1; 2]) "1 3 1 2 \n";;

(* check expect for string_of_matrix *)
check_expect (string_of_matrix []) "" ;;
check_expect (string_of_matrix [[0]]) "0 \n" ;;
check_expect (string_of_matrix prac_board1) ("0 0 0 1 0 0 0 \n0 0 0 1 0 0 2 "^
  "\n0 0 0 1 0 0 1 \n0 0 0 1 0 1 1 \n2 1 2 1 2 1 1 \n") ;;
check_expect (string_of_matrix prac_board2) ("0 0 0 1 0 0 1 \n0 0 0 1 0 0 2 "^
  "\n0 0 0 1 0 0 1 \n0 0 0 1 0 1 1 \n2 1 2 1 2 1 1 \n") ;;

(* check expects for string of state *)
check_expect (string_of_state ((State(Win(P1), prac_board1)))) ("Player 2 "^
  "loses! The final board is \n0 0 0 1 0 0 0 \n0 0 0 1 0 0 2 \n0 0 0 1 0 0 1 "^
    "\n0 0 0 1 0 1 1 \n2 1 2 1 2 1 1 \n") ;;
check_expect (string_of_state ((State(Draw, prac_board1)))) ("Draw. The final "^
  "board is \n0 0 0 1 0 0 0 \n0 0 0 1 0 0 2 \n0 0 0 1 0 0 1 \n0 0 0 1 0 1 1 "^
    "\n2 1 2 1 2 1 1 \n") ;;
check_expect (string_of_state ((State(Ongoing(P1), prac_board1)))) ("It is "^
  "Player 1's turn. The current board is \n0 0 0 1 0 0 0 \n0 0 0 1 0 0 2 "^
    "\n0 0 0 1 0 0 1 \n0 0 0 1 0 1 1 \n2 1 2 1 2 1 1 \n") ;;

(* check_expect for string_of_move *)
check_expect (string_of_move (Move(1))) "Column 1" ;;
check_expect (string_of_move (Move(8))) "Column 8" ;;
check_expect (string_of_move (Move(100))) "Column 100" ;;

(* check expects for make_list *)
check_expect (make_list 0) [];;
check_expect (make_list 8) [0; 0; 0; 0; 0; 0; 0; 0];;
check_expect (make_list 4) [0; 0; 0; 0];;

(* check expects for make matrix *)
check_expect (make_matrix (0, 0)) [] ;;
check_expect (make_matrix (0, 8)) [] ;;
check_expect (make_matrix (1, 1)) [[0]] ;;
check_expect (make_matrix (3, 4)) [[0; 0; 0; 0]; [0; 0; 0; 0]; [0; 0; 0; 0]];;

(* check expect for initial_board *)
check_expect (initial_board) [[0; 0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0; 0];
  [0; 0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0; 0]; [0; 0; 0; 0; 0; 0; 0]];;

(* check expects for legal moves *)
check_expect (legal_moves (State(Ongoing(P1), prac_board1)))
  [Move 1; Move 2; Move 3; Move 5; Move 6; Move 7] ;;
check_expect (legal_moves (State(Ongoing(P1), prac_board2)))
  [Move 1; Move 2; Move 3; Move 5; Move 6] ;;
check_expect (legal_moves (State(Ongoing(P1), prac_board7))) [] ;;

(* check expects for find_row *)
check_expect (find_row (prac_board1, 2, 1)) 4;;
check_expect (find_row (prac_board1, 7, 1)) 1;;
check_expect (find_row (prac_board1, 6, 1)) 3;;
check_expect (find_row (prac_board1, 1, 1)) 4;;
check_expect (find_row (prac_board1, 3, 1)) 4;;
check_error (fun () -> (find_row (prac_board1, 4, 1))) "column full";;

(* check expects for token *)
check_expect (token P1) 1 ;;
check_expect (token P2) 2 ;;

(* check expects for insert *)
check_expect (insert (P2, (List.hd prac_board5), 1)) [2; 0; 0; 0; 1; 0; 0] ;;
check_expect (insert (P1, (List.hd initial_board), 4)) [0; 0; 0; 1; 0; 0; 0] ;;
check_expect (insert (P2, (List.hd initial_board), 7)) [0; 0; 0; 0; 0; 0; 2] ;;

(* check expect for move_to_board *)
check_expect (move_to_board (P1, prac_board1, Move(1))) [[0; 0; 0; 1; 0; 0; 0];
  [0; 0; 0; 1; 0; 0; 2]; [0; 0; 0; 1; 0; 0; 1]; [1; 0; 0; 1; 0; 1; 1];
    [2; 1; 2; 1; 2; 1; 1]] ;;
check_expect (move_to_board ( P2, prac_board1, Move(7))) [[0; 0; 0; 1; 0; 0; 2];
 [0; 0; 0; 1; 0; 0; 2]; [0; 0; 0; 1; 0; 0; 1]; [0; 0; 0; 1; 0; 1; 1];
    [2; 1; 2; 1; 2; 1; 1]] ;;
check_error (fun () -> (move_to_board ( P2, [], Move(4)))) 
"incorrect row for identified for move" ;;

(* check expects for vert_flip *)
check_expect (vert_flip [[1]]) [[1]];;
check_expect (vert_flip [[1 ; 3 ; 5]]) [[5 ; 3 ; 1]];;
check_expect (vert_flip [[2 ; 1] ; [1 ; 3]]) [[1 ; 2] ; [3 ; 1]];;
check_expect (vert_flip [[1 ; 3 ; 5] ; [4 ; 3 ; 5]])
                        [[5 ; 3 ; 1] ; [5 ; 3 ; 4]];;
check_expect (vert_flip [[1] ; [3] ; [4] ; [8]]) [[1] ;[3] ; [4] ; [8]];;

(* check expects for transpose *)
check_expect (transpose [[4; 5; 6]; [3; 5; 4]; [4; 3; 1]]) [[4; 3; 4];
  [5; 5; 3]; [6; 4; 1]];;
check_expect (transpose [[1]]) [[1]] ;;
check_expect (transpose [[1] ; [4] ; [8]]) [[1 ; 4 ; 8]] ;;
check_expect (transpose [[1 ; 3] ; [1 ; 4]]) [[1 ; 1] ; [3 ; 4]] ;;
check_expect (transpose [[1 ; 3] ; [1 ; 4] ; [3 ; 4]])
                        [[1 ; 1 ; 3] ; [3 ; 4 ; 4]] ;;
check_expect (transpose [[1 ; 3 ; 8] ; [7 ; 2 ; 9]])
                        [[1 ; 7] ; [3 ; 2] ; [8 ; 9]] ;;
check_expect (transpose [[1 ; 2 ; 3] ; [4 ; 5 ; 6] ; [7 ; 8 ; 9]])
                        [[1 ; 4 ; 7] ; [2 ; 5 ; 8] ; [3 ; 6 ; 9]] ;;

(* check expects for horiz_win *)
check_expect (horiz_win ([0; 0; 0; 0; 1; 1; 0], 0)) None;;
check_expect (horiz_win ([0; 1; 1; 1; 1; 1; 0], 0)) (Some "win by player1");;
check_expect (horiz_win ([2; 2; 2; 1; 1; 1; 0], 0)) None;;
check_expect (horiz_win ([0; 2; 2; 2; 2; 1; 0], 0)) (Some "win by player2");;
check_expect (horiz_win ([0; 2; 2; 1; 1; 1; 1], 0)) (Some "win by player1");;

(* check expects for diagonal *)
check_expect (diagonal (prac_board5, 6)) [0; 0; 2; 1; 2];;
check_expect (diagonal (prac_board5, 7)) [2; 2; 2; 1];;
check_expect (diagonal ((transpose prac_board5), 3)) [0; 0; 0; 0];;
check_expect (diagonal ((transpose prac_board5) ,4)) [2; 0; 0; 1; 1];;
check_expect (diagonal ((transpose prac_board5) ,5)) [1; 1; 1; 1; 0];;
check_expect (diagonal ((transpose prac_board5) ,6)) [2; 1; 2; 0; 0];;
check_expect (diagonal ((transpose prac_board5) ,7)) [1; 2; 2; 2];;

(* check expects for diag_number *)
check_expect (diag_number (make_matrix (4,5))) 4 ;;
check_expect (diag_number (make_matrix (7,9))) 11;;
check_expect (diag_number (make_matrix (4,4))) 3;;
check_expect (diag_number (make_matrix (4,1))) 0;;
check_expect (diag_number (make_matrix (7,5))) 7;;

(* check expects for check_diagonal *)
check_expect (check_diagonals (prac_board4, (diag_number (prac_board4))))
   None;;
check_expect (check_diagonals (prac_board8, (diag_number (prac_board8))))
  (Some "win by player1");;
check_expect (check_diagonals (prac_board7, (diag_number (prac_board7)))) 
  None;;
check_expect (check_diagonals (prac_board10, (diag_number (prac_board10))))
  None;;
check_expect (check_diagonals
  ((vert_flip prac_board8), (diag_number (prac_board8)))) None;;
check_expect (check_diagonals
  ((vert_flip prac_board7), (diag_number (prac_board7))))
    (Some "win by player2");;
check_expect (check_diagonals ((vert_flip prac_board10),
  (diag_number (prac_board10)))) (Some "win by player1");;

(* check epxects for win (only vertical and horizantle wins)) *)
check_expect (win prac_board4) None;;
check_expect (win prac_board5) None;;
check_expect (win prac_board6) (Some "win by player2");;
check_expect (win (transpose prac_board4)) None;;
check_expect (win (transpose prac_board5)) (Some "win by player1");;
check_expect (win (transpose prac_board6)) None;;

(* check epxects for any win *)
check_expect (any_win prac_board4) None;;
check_expect (any_win prac_board7) (Some "win by player2") ;;
check_expect (any_win prac_board8) (Some "win by player1");;
check_expect (any_win prac_board9) (Some "win by player2");;
check_expect (any_win prac_board10) (Some "win by player1");;
check_expect (any_win prac_board11) None;;

(* check_expects for game_status *)
check_expect (game_status (State(Ongoing(P1), prac_board4))) (Ongoing(P1));;
check_expect (game_status (State(Ongoing(P2), prac_board5))) (Win(P1));;
check_expect (game_status (State(Ongoing(P1), prac_board6))) (Win(P2));;
check_expect (game_status (State(Ongoing(P1), prac_board6))) (Win(P2));;

(* check expcts for next_state *)
check_expect (next_state (State(Win(P1), prac_board5)) (Move(0)))
  (State (Win(P1), prac_board5));;
check_expect (next_state (State(Ongoing(P1), prac_board4)) (Move(4)))
  (State (Win(P1), (move_to_board (P1, prac_board4, Move 4)))) ;;
check_expect (next_state (State(Ongoing(P2), prac_board4)) (Move(4)))
  (State (Ongoing(P1), (move_to_board (P2, prac_board4, Move 4))));;
check_expect (next_state (State(Draw, prac_board7)) (Move(4)))
  (State (Draw, prac_board7));;

(* check expcts for move_of_string *)
check_expect (move_of_string ("1")) (Move(1)) ;;
check_expect (move_of_string ("8")) (Move(8)) ;;
check_expect (move_of_string ("100")) (Move(100)) ;;

(* check expects for horiz_three *)
check_expect (horiz_three(List.hd initial_board, 0, 1, 4)) 0 ;;
check_expect (horiz_three(List.hd initial_board, 0, 2, 4)) 0 ;;
check_expect (horiz_three(List.hd prac_board5, 0, 1, 4)) 1 ;;
check_expect (horiz_three(List.hd (List.tl prac_board5), 0, 2, 4)) 0 ;;
check_expect (horiz_three([0; 1; 1; 1; 2; 2; 1], 0, 1, 4)) 3 ;;
check_expect (horiz_three([1; 0; 1; 1; 2; 2; 1], 0, 1, 4)) 3 ;;
(* check_expects for count *)
check_expect (count (initial_board, 1)) 0 ;;
check_expect (count (initial_board, 2)) 0 ;;
check_expect (count (prac_board1, 1)) 7 ;;
check_expect (count (prac_board2, 2)) 0 ;;
check_expect (count (prac_board4, 2)) 1 ;;

(* check_expects for diag_count *)
check_expect (diag_count (initial_board, (diag_number initial_board), 1)) 0;;
check_expect (diag_count (prac_board1, (diag_number prac_board1), 1)) 5;;
check_expect (diag_count (prac_board5, (diag_number prac_board5), 1)) 1002 ;;
check_expect (diag_count (prac_board6, (diag_number prac_board6), 1)) 5 ;;
check_expect (diag_count (prac_board10, (diag_number prac_board10), 1)) 3;;

(* check_expects for estimate_value1 *)
check_expect (estimate_value1(initial_board, 1)) 0 ;;
check_expect (estimate_value1(prac_board1, 1)) 1024 ;;
check_expect (estimate_value1(prac_board3, 1)) 4 ;;
check_expect (estimate_value1(prac_board6, 1)) 19 ;;
check_expect (estimate_value1(prac_board10, 1)) 1018 ;;

(* check_expects for estimate_value *)
check_expect (estimate_value (State(Ongoing(P1), initial_board))) 0. ;;
check_expect (estimate_value (State(Ongoing(P1), prac_board2))) (-10000000.);;
check_expect (estimate_value (State(Ongoing(P1), prac_board4))) (17.) ;;
check_expect (estimate_value (State(Win(P1), prac_board6))) (10000000.) ;;




