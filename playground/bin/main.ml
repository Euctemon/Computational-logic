let ctx = Z3.mk_context []
let slv = Z3.Solver.mk_simple_solver ctx

let a = Z3.Arithmetic.Integer.mk_const_s ctx "a"
let () = print_endline "Hello, World!"