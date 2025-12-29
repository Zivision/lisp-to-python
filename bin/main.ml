open Lisp_to_python

let test_cases = [
  "42";
  "(+ 1 2)";
  "(let ((x 5)) x)";
  "(lam (x y) (+ x y))";
  "(if (> x 0) x (- x))";
]

let () =
  List.iter (fun input ->
    try
      Printf.printf "Input:  %s\n" input;
      let ast = Parser.parse input in
      Printf.printf "Output: %s\n\n" (Ast.string_of_expr ast)
    with Parser.ParseError msg ->
      Printf.printf "Error: %s\n\n" msg
  ) test_cases
