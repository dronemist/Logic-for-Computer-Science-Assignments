open Formula;;
open Bdd;;
open Program;;
open BDD;;

(* Returns combination of k in list *)
let rec get_comb list k = if k <= 0 then [[]]
                                   else (
                                     match list with 
                                     | [] -> []
                                     | x::xs -> 
                                     let with_first = List.map (fun l -> OprUnary(NOT, Variable(x))::l) (get_comb xs (k-1)) in 
                                     let without_first = (get_comb xs (k)) in 
                                    without_first @ with_first
                                   )  
;;

(* Takes OR of all elements in the list *)
let rec take_or list = match list with
[] -> Constant(false)
| [x] -> x
| x::xs -> OprBinary(OR, x, take_or xs)
;;

(* Takes AND of all elements in the list *)
let rec take_and list = match list with 
[] -> Constant(true)
| [x] -> x
| x::xs -> OprBinary(AND, x, take_and xs)
;;

(* Convert from list in CNF to bexpr *)
let rec convert_to_bexpr cnf = let l1 = (List.map take_or cnf) in 
                                (take_and l1)
;;

(* Take of the variables in list *)
let take_not list = 
  let temp l = OprUnary(NOT, l) in 
  let l1 = List.map temp list in 
  l1
;;

(* Take of the variables in list *)
let make_var list = 
  let temp l = Variable(l) in 
  let l1 = List.map temp list in 
  l1
;;


(* Get row elements *)
let row board_size i j = 
  let rec get_row board_size row col curr_col = 
    if board_size <= curr_col then []                          
    else if col == curr_col then (get_row board_size row col (curr_col + 1))
    else ("c" ^ (string_of_int row) ^ (string_of_int curr_col))::
    (get_row board_size row col (curr_col+1))
  in (get_row board_size i j 0)  
;;

(* Get column elements *)
let col board_size i j = 
  let rec get_col board_size row col curr_row = 
    if board_size <= curr_row then []                          
    else if row == curr_row then (get_col board_size row col (curr_row + 1))
    else ("c" ^ (string_of_int curr_row) ^ (string_of_int col))::
    (get_col board_size row col (curr_row+1))
  in (get_col board_size i j 0)  
;;

(* Get diagonal elements in top-left to bottom-right direction *)
let diag_r board_size i j = 
  let rec get_diag_r board_size row col k = (
    if board_size <= k then []                          
    else if k == row || k < 0 || board_size <= (col + k - row) || (col + k - row) < 0 then (get_diag_r board_size row col (k+1))
    else ("c" ^ (string_of_int k) ^ (string_of_int (col + k - row)))::
    (get_diag_r board_size row col (k+1))
  )
  in (get_diag_r board_size i j 0)  
;;

(* Get diagonal elements in top-right to bottom-left direction *)
let diag_l board_size i j = 
  let rec get_diag_l board_size row col k = 
    if board_size <= k then []                          
    else if k == row || k < 0 || board_size <= (col + row - k) || (col + row - k) < 0 then (get_diag_l board_size row col (k+1))
    else ("c" ^ (string_of_int k) ^ (string_of_int (col + row - k) ))::
    (get_diag_l board_size row col (k+1))
  in (get_diag_l board_size i j 0)  
;;

(* Constraints for atleast one queen per row *)
let get_rows board_size = 
  let rec rows board_size i = (
    if i == board_size then []
    else 
    let l1 = (row board_size i 0) in 
    let l2 = ("c" ^ (string_of_int i) ^ (string_of_int 0)) :: l1 in 
    (take_or (make_var l2)) :: (rows board_size (i + 1))
  )
  in (rows board_size 0)
;;

(* Constraints fiven the function f *)
let cnst f board_size = 
  let rec temp board_size i = (
    if i == board_size then []
    else
    let rec temp2 board_size i j = (
      if j == board_size then []
      else 
      let l1 = (make_var (f board_size i j)) in 
      let l2 = (take_and (take_not l1)) in 
      let var = Variable(("c" ^ (string_of_int i) ^ (string_of_int j))) in 
      OprBinary(IFTHEN, var, l2) :: (temp2 board_size i (j+1))
    ) in 
    (temp2 board_size i 0) @ (temp board_size (i+1))
  ) in 
  (temp board_size 0)
;;

(* Row constraints *)
let row_cnst board_size = cnst row board_size;;

(* Row constraints *)
let col_cnst board_size = cnst col board_size;;

(* Row constraints *)
let diag_r_cnst board_size = cnst diag_r board_size;;

(* Row constraints *)
let diag_l_cnst board_size = cnst diag_l board_size;;

(* Get order for bdd construction *)
let get_order board_size = 
  let rec temp board_size i = (
    if i == board_size then []
    else
    let rec temp2 board_size i j = (
      if j == board_size then []
      else 
      let var = ("c" ^ (string_of_int i) ^ (string_of_int j)) in 
      var :: (temp2 board_size i (j+1))
    ) in 
    (temp2 board_size i 0) @ (temp board_size (i+1))
  ) in 
  (temp board_size 0)
;;

(* 	The type of n_queen is int -> string list, where int is the 
	size of board. The solution is represented as list of strings, 
	where each string denotes the position of a queen in the solution. 
	The position is a string 'c' (lower case c without quotes) 
	appended with two single digit integers i and j, where i and j 
	are row and column numbers respectively, starting from 0. 
	For example, the string for cell in row 0 and column 4 should be c04. 
*)
let n_queen board_size = 
  let l1 = (get_rows board_size) in 
  let l2 = (row_cnst board_size) in
  let l3 = (col_cnst board_size) in 
  let l4 = (diag_l_cnst board_size) in 
  let l5 = (diag_r_cnst board_size) in 
  let l_fin = (take_and (l1@l2@l3@l4@l5)) in
  let order = (get_order board_size) in 
  let bdd = (bddFromExpr l_fin order) in 
  (bdd)
;;

(*	The type of  knight is int -> int -> int -> string list, where
	first int is board size, second and third ints represent the
	row and column number (starting from 0) respectively of 
	initial position of the knight on the board
	The output to this function should be a sequence of strings.
	The first element in the sequence should be cell name 
	corresponding to the initial position of the knight. 
	Each subsequent string in the sequence should represent the 
	next cell visited by the knight. 
*)
let knight board_size init_row init_col = raise BDD.Not_implemented ;;	

