open Printf
open Minisat


(* Creating an instance of a solver and stock of variables
   Literals have to be indexed from 1 and not from 0 *)
let board_size = 5
let latin_solve = create ()
let vars = Array.init (board_size * board_size * board_size) (fun x -> Lit.make (x+1))


(* Representing latin square problem as a predicate on a "cube"
   Variable a = (i,j,k) is true when the number k is placed in position (i,j) *)
let to_coord var =
    let a = var mod (board_size * board_size) in
    let b = var / board_size / board_size in
    (a / board_size, a mod board_size, b)

let from_coord (i,j,k) = k * board_size * board_size + i * board_size + j


(* Clauses ensuring that a number is at most once in a given row and a given column *)
let row var =
    let (_,j,k) = to_coord var in
    let temp = List.init board_size (fun x -> from_coord (x,j,k)) in
    List.filter (fun i -> i != var) temp

let column var =
    let (i,_,k) = to_coord var in
    let temp = List.init board_size (fun x -> from_coord (i,x,k)) in
    List.filter (fun i -> i != var) temp

let num_clauses var =
    List.iter (fun i -> add_clause_a latin_solve [| Lit.neg vars.(var); Lit.neg vars.(i)|]) (row var);
    List.iter (fun i -> add_clause_a latin_solve [| Lit.neg vars.(var); Lit.neg vars.(i)|]) (column var)
let rowcol_clauses =
    for i = 0 to (board_size * board_size * board_size - 1) do
        num_clauses i;
    done


(* Clauses ensuring a number is placed somewhere in each row *)
let existence num row = Array.init board_size (fun x -> from_coord (row,x,num))

let existence_clauses =
    for row = 0 to board_size - 1 do
        for num = 0 to board_size - 1 do
            add_clause_a latin_solve (Array.map (fun i -> vars.(i)) (existence num row))
        done
    done


(* Clauses ensuring that at most one number occupies a given position *)
let maximality num row col =
    let var = from_coord (row,col,num) in
    let pos_list = List.filter (fun i -> i != num) (List.init board_size (fun i -> i)) in
    List.iter (fun i -> 
        let j = from_coord (row,col,i) in
        add_clause_a latin_solve [|Lit.neg vars.(var) ; Lit.neg vars.(j)|]) pos_list

let maximality_clauses =
    for i = 0 to board_size - 1 do
        for j = 0 to board_size - 1 do
            for k = 0 to board_size - 1 do
                maximality i j k
            done
        done
    done


(* Sample solution writer *)
let sol_writer state =
    let formatter (i,j,k) = printf "%d %d %d\n" i j k in
    let filter_fun index var = if (value state var = V_true) then formatter (to_coord index) else () in
    Array.iteri (fun index var -> filter_fun index var) vars


(* Computing solution *)
let () =
    rowcol_clauses;
    existence_clauses;
    maximality_clauses;
    solve latin_solve;
    sol_writer latin_solve

(* for obtaining a list of all generated clauses passed to the solver
    Debug.to_dimacs_file latin_solve "/home/dave/comp_logic/latinsquare/complete_clause.txt" *)