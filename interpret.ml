let ( *> ) f g x = g (f x)

let rec parse lexbuf =
  let result = Parse.prog Lexer.read lexbuf
  in match result with
  | Some v -> v :: (parse lexbuf)
  | None -> []

let env_global = Eval.create_env ()

let interpret ?(loop=false) str =
  env_global := Lexing.from_string *> parse *> Eval.interpret ~loop !env_global @@ str

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

let read_stdlib () =
  let ic = open_in "./stdlib/library.ml" in
  let str = really_input_string ic (in_channel_length ic)
  in
  interpret str;
  close_in ic

let () =
  interpret {|
    (1 + 2 * 3) * 6;;
    fun x -> fun y -> fun z -> (x y) (z (y+1));;
    let x = fun u -> fun v -> u v in fun u -> u x;;
    let rec f x = if x = 0 then 0 else x + f (x-1) in f;;
    let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f (* tail recursive *);;
    let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f 10 0;;
  |};
  interpret {|
    let x = 42;;
    let sum = fun x -> let rec f x = let rec g a = if x=0 then a else f (x-1) (a+x) in g in f x 0 (* sum 1...x *);;
    let result = x + sum 10;;
    result;;
  |};
  read_stdlib ();
  interpret {|
    pow 2 10;; (* utilize stdlib *)
  |};
  interpreter stdin
