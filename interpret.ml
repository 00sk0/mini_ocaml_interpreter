let ( *> ) f g x = g (f x)

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let interpret ?(loop=false) str =
  Lexing.from_string *> parse *> Eval.interpret ~loop @@ str

let interpreter ic =
  let rec loop input =
    try
      let input = input ^ "\n" ^ (input_line ic) in
      interpret ~loop:true input;
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
    fun x -> fun y -> fun z -> (x y) (z (y+1));;
    let x = fun u -> fun v -> u v in fun u -> u x;;
  |};
  (* interpret {|
    1 + 2 * 3;;
    (fun f -> fun x -> f (f x));;
    fun x -> fun y -> fun z -> (x y) (z (y+1));;
    fun x -> fun y -> fun z -> z (x y = y z);;
    let x = fun u -> fun v -> u v in fun u -> u x;;
    let x = fun u -> fun v -> u v in fun u -> x u;;
  |}; *)
  interpreter stdin
