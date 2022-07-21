(* TOP OF THE STACK IS HEAD OF THE LIST *)
(* quit doesnt work *)

type command = Push of string | Pop | Add | Sub | Rem | Mul | Div | Neg | Swap | Quit | ToString | Println | Cat | AND | Or | Not


(* STUFF TO HANDLE CONVERTING push 1  into  ["push";"1"]     or   push " s c h r u t e " to ["push";" s c h r u t e "] *)
(*test if string is an int**)
let is_int s = try ignore (int_of_string s); true with _ -> false


(* 5 is the length of "push " *)
let lengthy (str: string)= String.length str - 5

(* gets the value thats being pushed *)
let grab_elemt (push: string) = String.sub push 5 (lengthy push)

(* ensures that -0 is eradicated *)
let get_elemt_no_neg_zeros (value: string): string = if (is_int (grab_elemt value)) then string_of_int(int_of_string (grab_elemt value))else (grab_elemt value)

(* returns true if a string s1 contains  another string s2 *)
let contains s1 s2 =
   try
     let len = String.length s2 in
     for i = 0 to String.length s1 - len do
       if String.sub s1 i len = s2 then raise Exit
     done;
     false
   with Exit -> true 

(* checks if a string is a push command and separates it, if it is *)
let check_for_push (str : string): string list = 
   match str with
   | "" -> []
   | s -> if(contains str "push") then ["push"; get_elemt_no_neg_zeros s] else [s]


let rec convert_all_pushes (listy : string list): string list = 
   match listy with
   | [] -> []
   | hd :: tl -> (check_for_push hd)@(convert_all_pushes tl) 
(* ----------------------------------------------------------------------------------------------------- *)


(* STRING HANDLERS *)
(* gets rid of the first and last characters of a string ---- useful when getting rid of those pesky quotes *)
let strip_both_chars str =
   match String.length str with
   | 0 | 1 | 2 -> ""
   | len -> String.sub str 1 (len - 2)

(* splits "yukalaylee,s" to  ["yukalaylee" ; "s"]    the s identifies it as a string*)
let split_str_thingy (str : string ): string list = 
   match str with
   | "" -> []
   | s -> (String.split_on_char '-' s)
(* grabs only "yukalaylee" *)
let get_first_str (listy : string list): string = 
   match listy with
   | [] -> ""
   | hd :: tl -> hd
(* adds -s to end of a string to identifiy it *)
let convert_to_str(str : string ): string  =
   match str with
   |""-> ":error:"
   |s -> String.concat "-" [s; "s"]

(**operations -------------------------------------------------------------------------------------------------------------*)

(* pushes element and checks if the element has quotes *)
let push_top(str : string ): string =
   match str with
   |"" -> ":error:"
   |s -> if (String.contains s '\"') then convert_to_str( strip_both_chars s ) else s
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

(* divide might be backwards *)
let div_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string  mid / int_of_string hd))::tl
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



(* adds ",s" to back of a value; so its an identifier *)
let toString_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::tl -> if (contains hd "-s")  then hd::tl else String.concat "-" [hd; "s"]:: tl

(* one element in the stack             -> push the element back and push :error:
   the stack is empty                   -> push :error: onto the stack
   the top two elements are not strings -> push the elements back onto the stack, and then push :error: *)
let cat_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> if((contains hd "-s") && (contains mid "-s")) then 
                        (String.concat " " [(get_first_str (split_str_thingy hd)); mid]):: tl  else "error"::hd::mid::tl
   |hd::tl -> ":error:"::hd::tl




   (* stack structure is like backwards compared to order of commands *)
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
                        | "toString" -> (convert_to_commands (mid :: tl))@[ToString]
                        | "println" -> (convert_to_commands (mid :: tl))@[Println]
                        | "quit" -> (convert_to_commands (mid :: tl))@[Quit]
                        | "cat" -> (convert_to_commands (mid :: tl))@[Cat]
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
                  | "toString" -> (convert_to_commands tl)@[ToString]
                  | "println" -> (convert_to_commands tl)@[Println]
                  | "quit" -> (convert_to_commands tl)@[Quit]
                  | "cat" -> (convert_to_commands tl)@[Cat]
                  | _ -> (convert_to_commands tl)  )
   | _ :: [] -> []







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
         let l = String.trim(input_line ic) in loop_read (l::acc)
      with
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

   (* separates push 1 into   [push ; 1] *)
   let listy = convert_all_pushes ls_str in
   
      (* might need to swap ["error";s] *)
   let println_op_HELPER(st : string): string list=
      (match st with
      |"" -> [":error:"]
      |s -> if (contains s "-s")  then (file_write_string (get_first_str (split_str_thingy s)); []) else ["error";s] ) in
   let println_op(st : string list): string list =
      (match st with
      |[] -> [":error:"]
      |hd::tl -> (println_op_HELPER hd)@tl ) in

   let rec executeCommand(command_lst : command list): string list = 
      (match command_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Push(x) -> (push_top x)::(executeCommand tl)
                     | Pop -> pop_top(executeCommand tl)
                     | Add -> add_op (executeCommand tl) 
                     | Sub -> sub_op (executeCommand tl) 
                     | Rem -> rem_op (executeCommand tl)
                     | Mul -> mul_op (executeCommand tl) 
                     | Div -> div_op (executeCommand tl)
                     | Neg -> neg_op(executeCommand tl)
                     | Swap ->swap_op(executeCommand tl) 
                     | ToString ->toString_op(executeCommand tl) 
                     | Println ->println_op(executeCommand tl) 
                     | Cat ->cat_op(executeCommand tl) 
                     | Quit -> executeCommand tl
                     | _ -> executeCommand tl ) 
      | _ :: [] -> []) in



   let command_list = executeCommand ( convert_to_commands listy ) in  
   
   let ben (str: string)= () in
   ben "sadfsd"


let () = (interpreter ("input1.txt" ,"output1.txt") ) 
let () = (interpreter ("input2.txt" ,"output2.txt") ) 
let () = (interpreter ("input3.txt" ,"output3.txt") ) 
let () = (interpreter ("input4.txt" ,"output4.txt") ) 
let () = (interpreter ("input5.txt" ,"output5.txt") ) 
let () = (interpreter ("input6.txt" ,"output6.txt") ) 
let () = (interpreter ("input7.txt" ,"output7.txt") ) 
let () = (interpreter ("input8.txt" ,"output8.txt") ) 
let () = (interpreter ("input9.txt" ,"output9.txt") ) 
let () = (interpreter ("input10.txt" ,"output10.txt") ) 