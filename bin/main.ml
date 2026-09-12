let () =
  let token = Bocaml.Token.Int 42 in
  match token with
  | Bocaml.Token.Int n ->
      Printf.printf "integer token: %d\n" n
  | _ ->
      print_endline "some other token"
