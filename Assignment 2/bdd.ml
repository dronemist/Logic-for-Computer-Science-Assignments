open Formula;;
open Program;;
(* open Graph;; *)

module BDD = struct
    type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
    type robdd = int * ((int, (string * int * int)) Hashtbl.t)
    * (((string * int * int), int) Hashtbl.t) * (string list) ;;
		        
		exception Not_implemented
    exception Invalid_operator
    exception Invalid_order_list
    exception No_solution

    (* Simplify the expression in an intelligent way *)
    let simplify bexpr = match bexpr with
    OprUnary (o, e) -> (match o with 
                      NOT -> (
                        match e with
                        Constant(value) -> Constant(not value);
                        | _ -> OprUnary (o, e) 
                      )
                      | _ -> raise Invalid_operator)
    | OprBinary (o, e1, e2) -> (match o with 
                      AND -> (
                        match (e1, e2) with 
                        (Constant(false), _) -> Constant(false)
                        | (_, Constant(false)) -> Constant(false)
                        | (Constant(v1), Constant(v2)) -> Constant(v1 && v2)
                        | _ -> OprBinary (o, e1, e2)
                      )
                      | OR -> (
                        match (e1, e2) with 
                        (Constant(true), _) -> Constant(true)
                        | (_, Constant(true)) -> Constant(true)
                        | (Constant(v1), Constant(v2)) -> Constant(v1 || v2)
                        | _ -> OprBinary (o, e1, e2)
                      )
                      | IFTHEN -> (
                        match (e1, e2) with 
                        (Constant(false), _) -> Constant(true)
                        | (_, Constant(true)) -> Constant(true)
                        | (Constant(v1), Constant(v2)) -> Constant(not(v1) || v2)
                        | _ -> OprBinary (o, e1, e2)
                      ) 
                      | IFF -> ( 
                        match (e1, e2) with 
                        (Constant(value1), Constant(value2)) -> Constant(value1 == value2)
                        | _ -> OprBinary (o, e1, e2)
                      )
                      | _ -> raise Invalid_operator)
    
    | OprTernary (o, e1, e2, e3) -> (match o with 
                      IFTHENELSE -> (
                        match e1 with 
                        Constant(value) -> if value then e2 else e3
                        | _ -> OprTernary (o, e1, e2, e3)
                      )
                      | _ -> raise Invalid_operator)
    | Constant(e) -> Constant(e)
    | Variable(e) -> Variable(e)
    ;; 

    (* Put the value of x in expr *)
    let rec put_val bexpr x value = match bexpr with 
    OprUnary (o, e) -> (simplify (OprUnary(o, (put_val e x value))))
    | OprBinary (o, e1, e2) -> (simplify (OprBinary (o, (put_val e1 x value), (put_val e2 x value))))
    | OprTernary (o, e1, e2, e3) -> (simplify (OprTernary (o, (put_val e1 x value), (put_val e2 x value), (put_val e3 x value))))
    | Constant(e) -> Constant(e)
    | Variable(e) -> if e = x then Constant(value) else Variable(e)
    ;; 

    let rec eval_expr bexpr = match bexpr with
    OprUnary (o, e) -> (match o with 
                      NOT -> (not(eval_expr e))
                      | _ -> raise Invalid_operator)
    | OprBinary (o, e1, e2) -> (match o with 
                      AND -> (
                        match (e1, e2) with 
                        (Constant(false), _) -> false
                        | (_, Constant(false)) -> false
                        | (Constant(v1), Constant(v2)) -> v1 && v2
                        | _ -> (eval_expr e1) && (eval_expr e2)
                      )
                      | OR -> (
                        match (e1, e2) with 
                        (Constant(true), _) -> true
                        | (_, Constant(true)) -> true
                        | (Constant(v1), Constant(v2)) -> v1 || v2
                        | _ -> (eval_expr e1) || (eval_expr e2)
                      )
                      | IFTHEN -> (
                        match (e1, e2) with 
                        (Constant(false), _) -> true
                        | (_, Constant(true)) -> true
                        | (Constant(v1), Constant(v2)) -> not(v1) || v2
                        | _ -> (not(eval_expr e1)) || (eval_expr e2)
                      ) 
                      | IFF -> (eval_expr e1) == (eval_expr e2)
                      | _ -> raise Invalid_operator)
    
    | OprTernary (o, e1, e2, e3) -> (match o with 
                      IFTHENELSE -> if (eval_expr e1) then (eval_expr e2) else (eval_expr e3)
                      | _ -> raise Invalid_operator)
    | Constant(e) -> e
    | Variable(e) -> raise Invalid_order_list
    ;; 

    (* Checking for reduced bdd *)
    let rec mk h1 h2 x l h = if l == h then l
                             else if (Hashtbl.mem h2 (x, l, h)) then (Hashtbl.find h2 (x, l, h))
                             else (
                                let len = (Hashtbl.length h1) in
                                (Hashtbl.add h1 len (x, l, h));
                                (Hashtbl.add h2 (x, l, h) len);
                                len
                             ) 
    ;;

    (* Int to bool conversion *)
    let int_to_bool x = if x then 1 else 0

    (* Construct entire bdd expr *)
    let rec bdd_from_expr bexpr order h1 h2 = match order with
    [] -> (int_to_bool (eval_expr bexpr))
    | x::xs -> let m1 = (put_val bexpr x false) in
               let m2 = (put_val bexpr x true) in 
               let v1 = bdd_from_expr m1 xs h1 h2 in 
               let v2 = bdd_from_expr m2 xs h1 h2 in 
               (mk h1 h2 x v1 v2) 
    ;; 

    (* Return the ROBDD formed *)
    let bddFromExpr bexpr order = let h1 = Hashtbl.create 2 in 
                                  let h2 = Hashtbl.create 2 in 
                                  (Hashtbl.add h1 0 ("0", -1, -1));
                                  (Hashtbl.add h1 1 ("1", -1, -1));
                                  (Hashtbl.add h2 ("0", -1, -1) 0);
                                  (Hashtbl.add h2 ("1", -1, -1) 1);
                                  (bdd_from_expr bexpr order h1 h2, h1, h2, order)
    ;;

    (* Return number of satisfying truth assignments *)
    let rec sat_count bdd = let (root, h1, h2, order) = bdd in
    ( match order with
      [] -> ( if root == 0 || root == 1 then root
              else raise Invalid_order_list
            )
      | x::xs -> ( let (var, l, h) = (Hashtbl.find h1 root) in 
                    if var = x then (sat_count (l, h1, h2, xs) + sat_count (h, h1, h2, xs))
                    else (2 * sat_count (root, h1, h2, xs))
                )
    )
    ;;

    (* Concats x to the list l *)
    let concat x l = x::l;;

    (* Return all satisfying truth assignment *)
		let rec all_sat bdd = let (root, h1, h2, order) = bdd in
    ( match order with
      [] -> ( 
              if root == 0 then []
              else if root = 1 then [[]]
              else raise Invalid_order_list
            )
      | x::xs -> ( let (var, l, h) = (Hashtbl.find h1 root) in 
                    if var = x then ( (all_sat (l, h1, h2, xs)) @ (List.map (concat var) (all_sat (h, h1, h2, xs))) )
                    else ( (all_sat (root, h1, h2, xs)) @ (List.map (concat x) (all_sat (root, h1, h2, xs))) )
                )
    );;

    (* Returns one satisfying assignment *)
    let rec any_sat bdd = 
    let (root, h1, h2, order) = bdd in
    let (var, l, h) = (Hashtbl.find h1 root) in
    ( 
      if root == 0 then raise No_solution
      else if root = 1 then []
      else if l == 0 then var::(any_sat (h, h1, h2, order))
      else any_sat (l, h1, h2, order)
    )
    ;;

    let rec to_dot_help list oc = match list with 
    [] -> ()
    | (root, (var, l, h))::xs -> 
                        Printf.fprintf oc "%d[label=%s] \n" root var;
                        if l != -1 then ( 
                          Printf.fprintf oc "%d -> %d" root l;  
                          Printf.fprintf oc "[color=red] [style=dashed] [label=\"0\"]\n"
                        );
                        if h != -1 then (
                          Printf.fprintf oc "%d -> %d" root h;
                          Printf.fprintf oc "[color=green] [label=\"1\"]\n"
                        );
                        to_dot_help xs oc;
    ;;
    let rec to_dot bdd = let oc = open_out "bdd.dot" in 
                          Printf.fprintf oc "digraph bdd { \n"; 
                          let (root, h1, h2, order) = bdd in
                          if root == 1 || root == 0 then (
                            Printf.fprintf oc "%d \n" root
                          ) 
                          else (
                          let l1 = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h1 [] in 
                          to_dot_help l1 oc
                          );
                          Printf.fprintf oc "} \n";
                          close_out oc;
    ;; 
		
end;;

(* let a = (Program.Constant true);;
a;; *)
