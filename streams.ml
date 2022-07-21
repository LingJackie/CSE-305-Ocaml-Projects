type 'a str = Cons of 'a * ('a stream) | Nil
and  'a stream = unit -> 'a str
   
exception Subscript
exception Empty
   
let head (s :'a stream) : 'a =
  match s () with
    Cons (hd,tl) -> hd
  | Nil -> raise Empty
                      
let tail (s :'a stream) : 'a stream =
  match s () with
    Cons (hd,tl) -> tl
  | Nil -> raise Empty
  
let null (s : 'a stream) =
  match s () with
    Nil -> true
  | _ -> false
  
let rec take (n: int) (s: 'a stream) : 'a list = 
  match n with
    n when n > 0 -> head s :: take (n - 1) (tail s)
  | 0 -> []
  | _ -> raise Subscript
  
let rec nth (n: int) (s: 'a stream) : 'a =
  match n with
    n when n > 0 -> nth (n - 1) (tail s)
  | 0 -> head s
  | _ -> raise Subscript

let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))
  
let rec filter (s: 'a stream) (f: 'a -> bool) : 'a stream = 
  if f (head s) 
  then fun () -> Cons (head s, filter (tail s) f)
  else filter (tail s) f

let rec sieve (s: int stream) : int stream =
  fun () -> Cons(head s, sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))

let rec fromn (n: int) = fun () -> Cons (n, fromn (n + 1))

(* take 10 (fib 0 1)     takes the first 10 numbers *)
let rec fib n m = fun () -> Cons (n, fib m (n+m))
  
(* implement the streams and functions below *)

(* let even : int -> bool = fun x -> true *)
let even (num : int): bool = if(num mod 2 == 0) then true else false
(* let odd  : int -> bool = fun x -> true *)
let odd (num : int): bool = if(num mod 2 == 0) then false else true


let squareInt (num:int) = num*num
let squares = map squareInt (fromn 1)


(* let fibs : int stream = fun () -> Nil      (fib 0 1) *)
let fibs = (fib 0 1)


(* let evenFibs : int stream = fun () -> Nil *)
let evenFibs = filter fibs even

(* let oddFibs : int stream = fun () -> Nil *)
let oddFibs = filter fibs odd

(* let primes : int stream = fun () -> Nil *)
let primes = sieve (fromn 2)





let subtract ((a:int) ,(b:int)) = a - b
let rec zippy (apple: 'a stream) (banan: 'b stream) (f: 'b * 'a -> 'c) =  fun () -> Cons (    (head banan, head apple,  (f ( (head banan), (head apple)) )   ), zippy (tail apple) (tail banan) f )

(* let rev_zip_diff : 'a stream -> 'b stream -> ('b * 'a -> 'c) -> ('b * 'a * 'c) stream =
  fun a b f -> fun () -> Nil *)
let rev_zip_diff (apple: 'a stream) (banan: 'b stream) (f: 'b * 'a -> 'c) = zippy apple banan f










(* let rec printGenList : 'a list -> ('a -> unit) -> unit =
  fun l f -> () *)
  let rec printGenList : 'a list -> ('a -> unit) -> unit =
    fun l f -> 
      match l with
      | [] -> ()
      | hd::tl -> f hd; printGenList tl f

let rec niceNPretty (lst: int list) : string =
  match lst with
  | [] -> ""
  | hd::tl -> string_of_int hd ^ " " ^ niceNPretty tl
let rec printList : int list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    let file_write_string string_val = Printf.fprintf oc "%s" string_val in
    file_write_string (   (niceNPretty l)) ;close_out oc
    
(* let rec niceNPretty (lst: int list) : string =
  match lst with
  | [] -> ""
  | hd::tl -> string_of_int hd ^ " " ^niceNPretty tl
let rec printList (lst: int list)(f:string) =
  fun l f ->
    let oc = open_out f in 
    let file_write_string string_val = Printf.fprintf oc "%s\n" string_val in
      match lst with
      | [] -> 
      | hd::tl -> string_of_int hd ^ " " ^niceNPretty tl
    file_write_string (niceNPretty lst)  *)

  

let rec stringPairsYay (lst: (int * int) list ) : string =
  match lst with
  | [] -> ""
  | hd::tl -> match hd with 
              | (uno, dos) -> "(" ^ string_of_int uno ^ ", " ^ string_of_int dos ^ ")" ^ " " ^ stringPairsYay tl
let rec printPairList : (int * int) list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    let file_write_string string_val = Printf.fprintf oc "%s" string_val in
    file_write_string (   (stringPairsYay l)) ;close_out oc

(* let () =  printList [2; 4; 6; 8] "printList.txt"  *)
(* let () =  printPairList [(2, 1); (3, 2); (4, 3)] "printList.txt" *)