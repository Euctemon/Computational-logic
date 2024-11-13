open Printf
open Minisat


(* Creating an instance of a solver and stock of variables
   Literals have to be indexed from 1 and not from 0 *)
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


(* Clauses for each queen and clauses that ensure some queens are actually placed in the grid
   Existence is modelled through assumption that there has to be a queen in each row *)
let position_clauses =
    for i = 0 to (board_size * board_size - 1) do
        List.iter (fun j -> add_clause_a queen_solve [| Lit.neg vars.(j); Lit.neg vars.(i) |]) (queen_reach i)
    done

let existence_clauses =
    for i = 0 to board_size - 1 do
        add_clause_a queen_solve (Array.init board_size (fun j -> vars.(j + i * board_size)))
    done


(* Sample solution writer *)
let sol_writer state =
    for i = 0 to board_size - 1 do
        for j = 0 to board_size - 1 do
            printf "%s " (string_of_value (value state vars.(j + board_size * i)))
        done;
        print_newline ();
    done


(* Computing solution *)
let () =
 position_clauses;
 existence_clauses;
 solve queen_solve;
 sol_writer queen_solve;