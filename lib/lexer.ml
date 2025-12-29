type token =
  | LParen
  | RParen
  | IntLit of int
  | Sym of string

let is_whitespace c =
  (* Checks if character is white space *)
  c = ' ' ||
  c = '\n' ||
  c = '\t' ||
  c = '\r'

let is_delimiter c =
  (* First checks if character is whitespace *)
  (* If not checks if it's paranthesis *)
  is_whitespace c ||
  c = '(' ||
  c = ')'

let tokenize input =
  let len = String.length input in
  let rec go i acc =
    (* Base case *)
    if i >= len then List.rev acc
    else
      let c = input.[i] in
      if is_whitespace c then go (i + 1) acc
      else if c = '(' then go (i + 1) (LParen :: acc)
      else if c = ')' then go (i + 1) (RParen :: acc)
      else
        (* Read a symbol or number *)
        let rec read_atom j =
          if j >= len || is_delimiter input.[j] then j
          else read_atom (j + 1)
        in
        let end_pos = read_atom i in
        let atom = String.sub input i (end_pos - i) in
        let token =
          try IntLit (int_of_string atom)
          with Failure _ -> Sym atom
        in
        go end_pos (token :: acc)
  in
  go 0 []
