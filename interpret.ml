let ( *> ) f g x = g (f x)

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let interpret ?(loop=false) =
  Lexing.from_string *> parse *> Eval.interpret ~loop

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
    (1 + 2 * 3) * 6;;
    fun x -> fun y -> fun z -> (x y) (z (y+1));;
    let x = fun u -> fun v -> u v in fun u -> u x;;
    let rec f x = if x = 0 then 0 else x + f (x-1) in f;;
    let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f (* tail recursive *);;
    let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f 10 0;;
  |};
  interpreter stdin
