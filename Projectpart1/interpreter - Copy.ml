let interpreter ( (input : string), (output : string )) : unit =
   (*takes in a pair of strings =>      string * string   *)
   
  (* Here we open an input channel for first argument, input, 
     and bind it to a variable ic so that we can refer it 
     later in loop_read function. *)
  let ic = open_in input in

  (* Use the second argument as file name to open an output channel,
     and bind it to variable oc for later reference. *)
  let oc = open_out output in 

  (* Helper function: file input function. It reads file line by line
     and return the result as a list of string.  *)
  let rec loop_read acc =
      (* We use try with to catch the End_of_file exception. *)
      try 
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in loop_read (l::acc)
      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the 
           head of old list. *)
      | End_of_file -> List.rev acc in

  (* Helper functions: file output function. It takes a bool/string value and
     write it to the output file.                                                              gonna need more of these for strings int and other stuff*)
  let file_write_bool bool_val = Printf.fprintf oc "%b\n" bool_val in
  let file_write_string string_val = Printf.fprintf oc "%s\n" string_val in

  (* This variable contains the result of input file from helper 
     function, loop_read. Please remember this is a list of string. 
     THIS IS WHERE ALL OF THE READ STUFF IS STORED *)
  let ls_str = loop_read [] in 

  (* ***** Code From Here, Replace () above and write your code ***** *)

  (*top of stack corresponds head of list*)

     (**-----------------------------------------------------------------------------------------------*) 
     
     

    
  



 (*has to be uppercase for some reason*)
 type command = Push of string | Pop | Add | Sub | Rem | Mul | Div | Neg | Swap | Quit | ToString | Println

(*test if string is an int**)
let is_int s = try ignore (int_of_string s); true with _ -> false

(* turns:  push 1 into   ["push";"1"] *)
let rec split_push (listy : string list): string list = 
   match listy with
   | [] -> []
   | hd :: tl -> (String.split_on_char ' ' hd)@(split_push tl) 

(**operations -------------------------------------------------------------------------------------------------------------*)
let pop_top(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: tl -> tl 


   (*empty will return error if only one element in stack push :error: onto top of stack*)
let sub_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                          (string_of_int (int_of_string mid - int_of_string hd))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl 

let add_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string hd + int_of_string mid))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl 

let mul_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string hd * int_of_string mid))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl

let div_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string hd / int_of_string mid))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl

let rem_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string mid mod int_of_string hd))::tl
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl 

let neg_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: tl ->  (if is_int hd  then 
                        (string_of_int (-(int_of_string hd )))::tl
                  else
                     (":error:")::hd :: tl )

let swap_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  mid::hd::tl
   |hd::tl -> (":error:"):: hd :: tl

   
(**converts the string list into command list backwards ------- You'll know y*)
(**convert_to_commands(["push";"1"; "push"; "2"; "pop"; "add";"push"; "6"; "pop";"pop";"pop"]) 
         command list = [Pop; Pop; Pop; Push "6"; Add; Pop; Push "2"; Push "1"]*)
let rec convert_to_commands(command_string_lst : string list): command list=
   match command_string_lst with
   | [] -> []
   | hd :: mid :: tl -> (match hd with
                        | "push" -> (convert_to_commands tl)@[Push(mid)]
                        | "pop" ->  (convert_to_commands (mid :: tl))@[Pop]
                        | "add" ->  (convert_to_commands (mid :: tl))@[Add]
                        | "sub" ->  (convert_to_commands (mid :: tl))@[Sub]
                        | "rem" ->  (convert_to_commands (mid :: tl))@[Rem]
                        | "mul" ->  (convert_to_commands (mid :: tl))@[Mul]
                        | "div" ->  (convert_to_commands (mid :: tl))@[Div]
                        | "neg" ->  (convert_to_commands (mid :: tl))@[Neg]
                        | "swap" -> (convert_to_commands (mid :: tl))@[Swap]
                        | _ -> (convert_to_commands (mid :: tl))  ) 
   | hd :: tl -> (match hd with
                  | "pop" ->  (convert_to_commands tl)@[Pop]
                  | "add" ->  (convert_to_commands tl)@[Add]
                  | "sub" ->  (convert_to_commands tl)@[Sub]
                  | "rem" ->  (convert_to_commands tl)@[Rem]
                  | "mul" ->  (convert_to_commands tl)@[Mul]
                  | "div" ->  (convert_to_commands tl)@[Div]
                  | "neg" ->  (convert_to_commands tl)@[Neg]
                  | "swap" -> (convert_to_commands tl)@[Swap]
                  | _ -> (convert_to_commands tl)  )
   | _ :: [] -> []



let rec executeCommand(command_lst : command list): string list = 
   match command_lst with
   | [] -> []
   | hd :: tl -> (match hd with
                  | Push(x) -> x::(executeCommand tl)
                  | Pop -> pop_top(executeCommand tl)
                  | Add -> add_op (executeCommand tl) 
                  | Sub -> sub_op (executeCommand tl) 
                  | Rem -> rem_op (executeCommand tl)
                  | Mul -> mul_op (executeCommand tl) 
                  | Div -> div_op (executeCommand tl)
                  | Neg -> neg_op(executeCommand tl)
                  | Swap ->swap_op(executeCommand tl) 
                  | _ -> executeCommand tl ) 
   | _ :: [] -> []

   (* let test = ["push";"1"; "push"; "2"; "pop"; "add";"push"; "6"; "pop";]  *)
   let test = ["push 1"; "push 2"; "pop"; "add";"push 6"; "pop";] 
   let spitted_lst = split_push test
   let convert = convert_to_commands spitted_lst
   let sh = executeCommand convert 

   let stack2 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "pop"; "add";"push"; "6"; "pop";"pop";"pop"]))  (*should be empty*)
   let stack3 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "add";"push"; "6"]))(* 6,3 *)
   let stack4 = executeCommand (convert_to_commands(["push";"1"; "push"; "2"; "add";"push"; "6"; "sub"]))
   let stack4 = executeCommand (convert_to_commands(["push";"1"; "push"; ":error:"; "push"; "6"; "sub"]))
(**-----------------------------------------------------------------------------------------------*) 

   (**pangram stuff*)
   let alphabet = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';
                  'm';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in


   (*Helper   if is a pangram then the and operation on all letters will be true 
         if there is a single false ie a letter is not there then all of it will be false
      String.contains s c tests if character c appears in the string s*)            
   let rec check (sentence : string) (alpha : char list): bool = 
      match alpha with 
      | [] -> true
      | hd :: tl -> String.contains sentence hd && check sentence tl in

   let rec bool_list (sentence_list : string list) (alpha : char list): bool list =
      match sentence_list with
      | [] -> []
      | hd :: tl -> check hd alpha :: bool_list tl alpha in

   let rec print_bool_list (b_lst : bool list) : unit =
      match b_lst with
      | [] -> ()
      | hd :: tl -> file_write hd; print_bool_list tl in
   
   let bl = bool_list ls_str alphabet in print_bool_list bl in
   interpreter ("input.txt", "output.txt")