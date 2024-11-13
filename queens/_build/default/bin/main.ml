open Printf
open Minisat

(* Creating an instance of a solver and stock of variables *)
let board_size = 6
let queen_solve = create ()
let vars = Array.init (board_size * board_size) (fun x -> Lit.make (x+1))

(* Computing all position a queen can reach *)

let row queen =
    let start = queen - (queen mod board_size) in
    List.init board_size (fun i -> i + start)

let column queen =
    let start = queen mod board_size in
    List.init board_size (fun i -> start + i * board_size)

let diag queen =
    let (a,b) = (queen mod board_size, queen / board_size) in
    let size = board_size - (max a b - min a b) in
    let start =  queen - (min a b * (board_size + 1)) in
    List.init size (fun i -> start + i * (board_size + 1))

let invdiag queen =
    let (a,b) = (queen mod board_size, queen / board_size) in
    let size = board_size - (max (a + b) (board_size - 1) - min (a + b) (board_size - 1)) in
    let start =  queen + (min a (board_size - b - 1) * (board_size - 1)) in
    List.init size (fun i -> start - i * (board_size - 1))

let queen_reach queen =
    let all_sides = List.concat_map (fun f -> f queen) [row; column; diag; invdiag] in
    List.filter (fun i -> i != queen) all_sides


(* Creating clauses*)

let pos_clauses =
    for i = 0 to (board_size * board_size - 1) do
        List.iter (fun j -> add_clause_a queen_solve [| Lit.neg vars.(j); Lit.neg vars.(i) |]) (queen_reach i)
    done

let row_clauses =
    for i = 0 to board_size - 1 do
        add_clause_a queen_solve (Array.init board_size (fun j -> vars.(j + i * board_size)))
    done

let vars_vals state =
    for i = 0 to board_size - 1 do
        for j = 0 to board_size - 1 do
            printf "%s " (string_of_value (value state vars.(j + board_size * i)))
        done;
        print_newline ();
    done

let () =
 pos_clauses;
 row_clauses;
 solve queen_solve;
 vars_vals queen_solve;
 (*
    let vars = Array.init 1 (fun x -> Lit.make (x+1))

let clause1 = [vars.(0)]
let clause2 = [Lit.neg vars.(0)]
let clause3 = [Lit.neg vars.(2); Lit.neg vars.(0)]
let clause4 = [vars.(0); Lit.neg vars.(1)]


let vars_vals state =
 let stringified = Array.map (fun x -> string_of_value (value state x)) vars
 in Array.fold_left (fun acc a -> String.concat " " [acc;a]) "" stringified

let () =
 add_clause_l queen_solve clause1;
 add_clause_l queen_solve clause2;
 add_clause_l queen_solve clause3;
 add_clause_l queen_solve clause4;
 solve queen_solve;
 print_endline (vars_vals queen_solve);
 print_endline (string_of_bool (okay queen_solve))
*)