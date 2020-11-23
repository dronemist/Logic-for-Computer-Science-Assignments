open Formula;;
open Program;;
open Graph;;

module BDD = struct
    type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
    type robdd = int * ((int, (string * int * int)) Hashtbl.t)
    * (((string * int * int), int) Hashtbl.t) * (string list) ;;
		        
		exception Not_implemented
    exception Invalid_operator
    exception Invalid_order_list
    exception No_solution

    (* Map for variable to value mappings *)
    module Rho = Map.Make(String);;

    (* Evaluate the expression *)
    let rec eval_expr bexpr rho = match bexpr with
    OprUnary (o, e) -> (match o with 
                      NOT -> (not(eval_expr e rho))
                      | _ -> raise Invalid_operator)
    | OprBinary (o, e1, e2) -> (match o with 
                      AND -> ((eval_expr e1 rho) && (eval_expr e2 rho))
                      | OR -> ((eval_expr e1 rho) || (eval_expr e2 rho))
                      | IFTHEN -> ((not(eval_expr e1 rho)) || (eval_expr e2 rho)) 
                      | IFF -> (let a1 = (eval_expr e1 rho) in
                                let a2 = (eval_expr e2 rho) in 
                                a1 == a2
                                )
                      | _ -> raise Invalid_operator)
    
    | OprTernary (o, e1, e2, e3) -> (match o with 
                      IFTHENELSE -> if (eval_expr e1 rho) then (eval_expr e2 rho) else (eval_expr e3 rho)
                      | _ -> raise Invalid_operator)
    
    | Constant(e) -> e
    | Variable(e) -> if (Rho.mem e rho) then (Rho.find e rho) 
                    else
                    (Printf.printf "%s" e;
                    raise Invalid_order_list)
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
    let rec bdd_from_expr bexpr order rho h1 h2 = match order with
    [] -> (int_to_bool (eval_expr bexpr rho))
    | x::xs -> let m1 = (Rho.add x false rho) in
               let m2 = (Rho.add x true rho) in 
               let v1 = bdd_from_expr bexpr xs m1 h1 h2 in 
               let v2 = bdd_from_expr bexpr xs m2 h1 h2 in 
               (mk h1 h2 x v1 v2) 
    ;; 

    (* Return the ROBDD formed *)
    let bddFromExpr bexpr order = let m1 = Rho.empty in 
                                  let h1 = Hashtbl.create 2 in 
                                  let h2 = Hashtbl.create 2 in 
                                  (Hashtbl.add h1 0 ("0", -1, -1));
                                  (Hashtbl.add h1 1 ("1", -1, -1));
                                  (Hashtbl.add h2 ("0", -1, -1) 0);
                                  (Hashtbl.add h2 ("1", -1, -1) 1);
                                  (bdd_from_expr bexpr order m1 h1 h2, h1, h2, order)
    ;;

    (* Return number of satisfying truth assignments *)
    let rec sat_count bdd = let (root, h1, h2, order) = bdd in
    ( match order with
      [] -> ( if root == 0 || root == 1 then root
              else raise Invalid_order_list
            )
      | x::xs -> ( let (var, l, h) = (Hashtbl.find h1 root) in 
                    if var == x then (sat_count (l, h1, h2, xs) + sat_count (h, h1, h2, xs))
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
                    if var == x then ( (all_sat (l, h1, h2, xs)) @ (List.map (concat var) (all_sat (h, h1, h2, xs))) )
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
                          let l1 = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h1 [] in 
                          to_dot_help l1 oc;
                          Printf.fprintf oc "} \n";
                          close_out oc;
    ;; 
		
end;;

(* let a = (Program.Constant true);;
a;; *)
