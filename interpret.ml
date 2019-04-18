let ( *> ) f g x = g (f x)

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let interpret =
  Lexing.from_string *> parse *> Eval.interpret

let interpreter ic =
  let rec loop input =
    try
      let input = input ^ "\n" ^ (input_line ic) in
      (* begin try
        interpret input;
      with Parse.Error -> loop input end; *)
      interpret input;
      loop ""
    with
    | End_of_file | Exit -> ()
    | e ->
      prerr_endline @@ "\027[31mError : " ^  Printexc.to_string e ^ "\027[0m";
      prerr_endline @@ Printexc.get_backtrace ();
      loop ""
  in loop ""

let () =
  interpret {|
    1 + 2 * 3;;
  |};
  interpreter stdin
