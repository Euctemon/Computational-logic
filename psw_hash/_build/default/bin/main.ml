
(* declaring context and initializing the solver solver *)
let ctx = Z3.mk_context []
let slv = Z3.Solver.mk_solver ctx None

(* variables *)
let x1 = Z3.BitVector.mk_const_s ctx "x1" 8
let x2 = Z3.BitVector.mk_const_s ctx "x2" 8
let x3 = Z3.BitVector.mk_const_s ctx "x3" 8
let x4 = Z3.BitVector.mk_const_s ctx "x4" 8
let vars = [x1; x2; x3; x4]

(* constants and shortcuts for operations *)
let zero = Z3.BitVector.mk_numeral ctx "0" 8
let one = Z3.BitVector.mk_numeral ctx "1" 8
let six = Z3.BitVector.mk_numeral ctx "6" 8
let ten = Z3.BitVector.mk_numeral ctx "10" 8
let eleven = Z3.BitVector.mk_numeral ctx "11" 8
let twofive = Z3.BitVector.mk_numeral ctx "25" 8

let eq' = Z3.Boolean.mk_eq ctx
let or' = Z3.Boolean.mk_or ctx
let not' = Z3.Boolean.mk_not ctx
let and' = Z3.Boolean.mk_and ctx
let le' = Z3.BitVector.mk_ule ctx
let xor' = Z3.BitVector.mk_xor ctx
let shift = fun b -> Z3.BitVector.mk_shl ctx b one

(* starting clauses *)
let exists_ten = or' (List.map (fun i -> eq' i ten) vars)
let exists_eleven = or' (List.map (fun i -> eq' i eleven) vars)
let le_twofive = and' (List.map (fun i -> le' i twofive) vars)
let eq_six = eq' six (List.fold_left (fun acc i -> xor' i (shift acc)) zero vars)
let starting_clauses = [exists_ten; exists_eleven; le_twofive; eq_six]

(* logging obtained passwords and adding them as clauses for next iteration of the solver *)
let char_of_bitvec model' var =
    let interp = Z3.Model.get_const_interp_e model' var in
    let to_char = fun i -> (Z3.Expr.simplify (Z3.BitVector.mk_bv2int ctx i true) None)
        |> Z3.Expr.to_string
        |> int_of_string
        |> (+) 97
        |> Char.chr
    in Option.map to_char interp

let log_password model' =
    vars
    |> List.map (fun v -> Option.value ~default:'_' (char_of_bitvec model' v))
    |> List.iter (fun v -> print_char v)
    |> print_newline

let password_clause model' =
    match List.map (fun i -> Z3.Model.get_const_interp_e model' i) vars with
    | [Some v1; Some v2; Some v3; Some v4] ->
        let sol = [eq' x1 v1; eq' x2 v2; eq' x3 v3; eq' x4 v4] in Some (not' (and' sol))
    | _ -> None

let rec gen_all_passwords solver' constrains =
    let _ = Z3.Solver.check solver' constrains in
    match Z3.Solver.get_model solver' with
    | None -> ()
    | Some m ->
        log_password m;
        Option.iter (fun c -> gen_all_passwords solver' (c::constrains)) (password_clause m)

(* running solver *)
let () = gen_all_passwords slv starting_clauses