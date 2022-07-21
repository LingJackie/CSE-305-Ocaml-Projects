open String
(* TOP OF THE STACK IS HEAD OF THE LIST *)




(* STUFF FOR BIND *)

(* insert a binding from key k to value v into  a "map" 
let tmp = [("","")]
let salem = insertToMap ("bottle", "slem",  tmp)             GOTTA CALL IT THIS WAY
ONLY A NAME CAN BE KEY
Have executeCommand take a map as a parameter*)

(* checks if key is in the map 
GOTTA CALL IT LIKE THIS: mapContains (k ,tl)*)
let rec mapContains ((k: string), (mappy:(string * string) list)): bool = 
   match mappy with
   | [] -> false
   | (key,value)::tl -> if ((compare key k) == 0) then true else mapContains (k ,tl)

let rec replaceMapValue( (k:string), (v:string) ,(mappy:(string * string) list) ): (string * string) list = 
   match mappy with
   | [] -> [("","")]
   | (key,value)::tl -> if ((compare key k) == 0) then (key ,v)::( replaceMapValue (k, v, tl) ) else (key,value)::( replaceMapValue (key ,value, tl) )

let insertToMap ( (k:string), (v:string) ,(mappy:(string * string) list) ): (string * string) list = 
   if ( mapContains (k , mappy) ) then
      (replaceMapValue (k, v, mappy))
   else
      (k,v)::mappy 

(* use the key to get the value  *)
let rec getMapValue ((k: string), (mappy:(string * string) list)): string = 
   match mappy with
   | [] -> ""
   | (key,value)::tl -> if ((compare key k) == 0) then value else getMapValue (k ,tl)

let popMap (mappy:(string * string) list): (string * string) list = 
   match mappy with
   | [] -> []
   | hd::tl -> tl



(* quit doesnt work *)

type command = Push of string | Pop | Add | Sub | Rem | Mul | Div | Neg | Swap | Quit | ToString | Println | Cat | And | Or | Not | Equal | LessThan | Bind | If | Let | End | Frick



(* compare returns 0 if they are equal *)
let is_booly (str: string): bool = ((compare str ":true:") == 0 || (compare str ":false:") ==0)

(* ------------STUFF TO HANDLE CONVERTING push 1  into  ["push";"1"]     or   push " s c h r u t e " to ["push";" s c h r u t e "]------------------ *)

(*test if string is an int**)
let is_int s = try ignore (int_of_string s); true with _ -> false

(* 5 is the length of the string: "push " *)
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


(* check if string is a name or not *)
let is_name (str: string): bool = if((is_int str) || (contains str "-s") || (is_booly str) || (contains str ":" ))then false else true
(* ----------------------------------------------------------------------------------------------------- *)


(* STRING HANDLERS *)
(* gets rid of the first and last characters of a string ---- useful when getting rid of those pesky quotes *)
let strip_both_chars str =
   match String.length str with
   | 0 | 1 | 2 -> ""
   | len -> String.sub str 1 (len - 2)



(* gets rid of the -s *)
let removeStringMarker (str : string ): string = if(contains str "-s") then String.sub str 0 ((String.length str) - 2) else str

(* adds -s to end of a string to identifiy it *)
let convert_to_str(str : string ): string  =
   match str with
   |""-> ":error:"
   |s -> String.concat "-" [s; "s"]




(**operations -------------------------------------------------------------------------------------------------------------*)

let pop_top(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: tl -> tl


   (*empty will return error if only one element in stack push :error: onto top of stack*)
let sub_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then
                          (string_of_int (int_of_string mid - int_of_string hd))::tl
                        
                        else if (is_int hd && mapContains (mid, mappy)) then
                          (if( is_int (getMapValue (mid, mappy))) then
                             (string_of_int ( int_of_string (getMapValue (mid, mappy)) - int_of_string hd  )   )::tl
                           else (":error:")::hd :: mid :: tl)
                        
                        else if (mapContains (hd, mappy) && is_int mid) then
                           (if( is_int (getMapValue (hd, mappy))) then
                              (string_of_int ( int_of_string mid -int_of_string (getMapValue (hd, mappy))    ))::tl
                           else (":error:")::hd :: mid :: tl)   
                           
                        else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (hd, mappy))   && is_int (getMapValue (mid, mappy))) then
                              (string_of_int (int_of_string (getMapValue (mid, mappy)) -  int_of_string (getMapValue (hd, mappy)) ))::tl
                           else (":error:")::hd :: mid :: tl)
                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl

let add_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then 
                           (string_of_int (int_of_string hd + int_of_string mid))::tl

                        else if (is_int hd && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (mid, mappy))) then
                              (string_of_int (  int_of_string hd + int_of_string (getMapValue (mid, mappy))   ))::tl
                            else (":error:")::hd :: mid :: tl)

                        else if (mapContains (hd, mappy) && is_int mid) then
                           (if( is_int (getMapValue (hd, mappy))) then
                              (string_of_int ( int_of_string (getMapValue (hd, mappy))  + int_of_string mid  ))::tl
                           else (":error:")::hd :: mid :: tl)
                        
                        else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (hd, mappy))   && is_int (getMapValue (mid, mappy))) then
                              (string_of_int (  int_of_string (getMapValue (hd, mappy)) + int_of_string (getMapValue (mid, mappy))   ))::tl
                           else (":error:")::hd :: mid :: tl)

                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl

let mul_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then
                           (string_of_int (int_of_string mid * int_of_string hd))::tl
                        
                        else if (is_int hd && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (mid, mappy))) then
                              (string_of_int ( int_of_string (getMapValue (mid, mappy)) * int_of_string hd  )   )::tl
                           else (":error:")::hd :: mid :: tl)
                        
                        else if (mapContains (hd, mappy) && is_int mid) then
                           (if( is_int (getMapValue (hd, mappy))) then
                              (string_of_int ( int_of_string mid * int_of_string (getMapValue (hd, mappy))    ))::tl
                           else (":error:")::hd :: mid :: tl)   
                           
                        else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (hd, mappy))   && is_int (getMapValue (mid, mappy))) then
                              (string_of_int (int_of_string (getMapValue (mid, mappy)) *  int_of_string (getMapValue (hd, mappy)) ))::tl
                           else (":error:")::hd :: mid :: tl)
                        else
                           (":error:")::hd :: mid :: tl )
|hd::tl -> (":error:"):: hd :: tl

(* divide might be backwards *)
let div_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then
                           (string_of_int (int_of_string mid / int_of_string hd))::tl

                        else if (is_int hd && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (mid, mappy))) then
                              (string_of_int ( int_of_string (getMapValue (mid, mappy)) / int_of_string hd  )   )::tl
                           else (":error:")::hd :: mid :: tl)

                        else if (mapContains (hd, mappy) && is_int mid) then
                           (if( is_int (getMapValue (hd, mappy))) then
                              (string_of_int ( int_of_string mid / int_of_string (getMapValue (hd, mappy))    ))::tl
                           else (":error:")::hd :: mid :: tl)   
                           
                        else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (hd, mappy))   && is_int (getMapValue (mid, mappy))) then
                              (string_of_int (int_of_string (getMapValue (mid, mappy)) /  int_of_string (getMapValue (hd, mappy)) ))::tl
                           else (":error:")::hd :: mid :: tl)
                        else
                           (":error:")::hd :: mid :: tl )
|hd::tl -> (":error:"):: hd :: tl

let rem_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  (if is_int hd && is_int mid then
                           (string_of_int (int_of_string mid mod int_of_string hd))::tl

                        else if (is_int hd && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (mid, mappy))) then
                              (string_of_int ( int_of_string (getMapValue (mid, mappy)) mod int_of_string hd  )   )::tl
                           else (":error:")::hd :: mid :: tl)

                        else if (mapContains (hd, mappy) && is_int mid) then
                           (if( is_int (getMapValue (hd, mappy))) then
                              (string_of_int ( int_of_string mid mod int_of_string (getMapValue (hd, mappy))    ))::tl
                           else (":error:")::hd :: mid :: tl)   
                           
                        else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                           (if( is_int (getMapValue (hd, mappy))   && is_int (getMapValue (mid, mappy))) then
                              (string_of_int (int_of_string (getMapValue (mid, mappy)) mod  int_of_string (getMapValue (hd, mappy)) ))::tl
                           else (":error:")::hd :: mid :: tl)

                        else
                           (":error:")::hd :: mid :: tl )
   |hd::tl -> (":error:"):: hd :: tl


let neg_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd :: tl ->  (if is_int hd  then
                        (string_of_int (-(int_of_string hd )))::tl
                  else if (mapContains (hd, mappy)) then
                     (if( is_int (getMapValue (hd, mappy))) then
                        (string_of_int (     -(int_of_string (getMapValue (hd, mappy)) )   )   )::tl
                     else (":error:")::hd :: tl )
                  else
                     ":error:"::hd :: tl )

let swap_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd :: mid :: tl ->  mid::hd::tl
   |hd::tl -> (":error:"):: hd :: tl



(* adds ",s" to back of a value; so its an identifier *)
let toString_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::tl -> if (contains hd "-s")  then hd::tl else (String.concat "-" [hd; "s"]):: tl

(* one element in the stack             -> push the element back and push :error:
   the stack is empty                   -> push :error: onto the stack
   the top two elements are not strings -> push the elements back onto the stack, and then push :error: *)
let cat_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> if((contains hd "-s") && (contains mid "-s")) then
                        (String.concat "" [(removeStringMarker mid); hd]):: tl  else ":error:"::hd::mid::tl
   |hd::tl -> ":error:"::hd::tl

(*
only one element in the stack                   push the element back and push :error:
the stack is empty                              push :error: onto the stack
either of the top two elements are not booleans push back the elements and push :error: *)
let and_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> 



                  (if ((is_booly hd) && (is_booly mid )) then
                     if( (compare hd mid) == 0 ) then hd::tl else ":false:"::tl

                  else if (is_booly hd && mapContains (mid, mappy)) then
                     (if( is_booly (getMapValue (mid, mappy))) then
                        if(   (      compare hd (getMapValue (mid, mappy))    ) == 0) then ":true:"::tl else ":false:"::tl
                     else (":error:")::hd :: mid :: tl)   

                  else if (mapContains (hd, mappy) && is_booly mid) then
                     (if( is_booly (getMapValue (hd, mappy))) then
                        if((      compare (getMapValue (hd, mappy)) mid    ) == 0) then ":true:"::tl else ":false:"::tl
                     else (":error:")::hd :: mid :: tl)   
                     
                  else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                     (if( is_booly (getMapValue (hd, mappy))   && is_booly (getMapValue (mid, mappy))) then
                        if( (   compare (getMapValue (hd, mappy)) (getMapValue (mid, mappy))    ) == 0) then ":true:"::tl else ":false:"::tl
                     else (":error:")::hd :: mid :: tl)

                  else
                     (":error:")::hd :: mid :: tl )
   |hd::tl -> ":error:"::hd::tl
(* one element in the stack                          push the element back and push :error:
   stack is empty                                    push :error: onto the stack
   either of the top two elements are not booleans   push back the elements and push :error: *)
let or_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> 
                  (if ((is_booly hd) && (is_booly mid )) then
                     if( ((compare hd ":false:") == 0) && ((compare mid ":false:") == 0)) then hd::tl else ":true:"::tl

                  else if (is_booly hd && mapContains (mid, mappy)) then
                     (if( is_booly (getMapValue (mid, mappy))) then
                        if( ((compare hd ":false:") == 0) && ((compare (getMapValue (mid, mappy)) ":false:") == 0)) then ":false:"::tl else ":true:"::tl
                     else (":error:")::hd :: mid :: tl)   

                  else if (mapContains (hd, mappy) && is_booly mid) then
                     (if( is_booly (getMapValue (hd, mappy))) then
                        if( ((compare (getMapValue (hd, mappy)) ":false:") == 0) && ((compare mid ":false:") == 0)) then ":false:"::tl else ":true:"::tl
                     else (":error:")::hd :: mid :: tl)   
                     
                  else if (mapContains (hd, mappy) && mapContains (mid, mappy)) then
                     (if( is_booly (getMapValue (hd, mappy))   && is_booly (getMapValue (mid, mappy))) then
                        if( ((compare (getMapValue (hd, mappy)) ":false:") == 0) && ((compare (getMapValue (mid, mappy)) ":false:") == 0)) then ":false:"::tl else ":true:"::tl  
                     else (":error:")::hd :: mid :: tl)

                  else
                     (":error:")::hd :: mid :: tl )
   |hd::tl -> ":error:"::hd::tl
let not_op((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |hd::tl ->  if(is_booly hd) then (
                  if( (compare hd ":false:") == 0) then ":true:"::tl else ":false:"::tl)
               else if (mapContains (hd, mappy)) then
                  (if( is_booly (getMapValue (hd, mappy))) then
                     (if( (compare (getMapValue (hd, mappy)) ":false:") == 0) then ":true:"::tl else ":false:"::tl)
                  else ":error:"::hd::tl  )
               else ":error:"::hd::tl

               
let equal_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> if((is_int hd) && (is_int mid )) then (
                        if((compare hd mid) == 0) then ":true:"::tl else ":false:"::tl)
                     else ":error:"::hd::mid::tl
   |hd::tl -> ":error:"::hd::tl
let lessThan_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |hd::mid::tl -> if((is_int hd) && (is_int mid )) then (
                        if(int_of_string mid < int_of_string hd) then ":true:"::tl else ":false:"::tl)
                     else ":error:"::hd::mid::tl
   |hd::tl -> ":error:"::hd::tl

(* pushes element and checks if the element has quotes 
errors if pushing a float ie 1.0
*)
let push_top(str : string ): string =
   match str with
   |"" -> ":a push error:"
   |s -> if (String.contains s '\"') then 
            convert_to_str( strip_both_chars s ) 
         else (
            if(contains s ".") then ":error:" else s
         )

(* removes the 2 elements and pushes :unit: to stack *)
let bind_op_stack((st : string list),(mappy:(string * string) list)): string list =
   match st with
   |[] -> [":error:"]
   |v::n::tl ->   if( is_name n && (compare (push_top v) ":error:") != 0) then (
                     if(mapContains (v, mappy) != true && is_name v) then
                        ":error:"::v::n::tl 
                     else
                        ":unit:"::tl )
                  else ":error:"::v::n::tl
   |hd::tl -> ":error:"::hd::tl 

let if_op(st : string list): string list =
   match st with
   |[] -> [":error:"]
   |val1::val2::":true:"::tl ->  val1::tl
   |val1::val2::":false:"::tl ->  val2::tl
   |val1::val2::val3::tl ->  ":error:"::val1::val2::val3::tl
   |hd::mid::tl -> ":error:"::hd::mid::tl              
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
                        | "and" -> (convert_to_commands (mid :: tl))@[And]
                        | "or" -> (convert_to_commands (mid :: tl))@[Or]
                        | "not" -> (convert_to_commands (mid :: tl))@[Not]
                        | "equal" -> (convert_to_commands (mid :: tl))@[Equal]
                        | "lessThan" -> (convert_to_commands (mid :: tl))@[LessThan]
                        | "bind" -> (convert_to_commands (mid :: tl))@[Bind]
                        | "if" -> (convert_to_commands (mid :: tl))@[If]
                        | "let" -> (convert_to_commands (mid :: tl))@[Let]
                        | "end" -> (convert_to_commands (mid :: tl))@[End]
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
                  | "and" -> (convert_to_commands tl)@[And]
                  | "or" -> (convert_to_commands tl)@[Or]
                  | "not" -> (convert_to_commands tl)@[Not]
                  | "equal" -> (convert_to_commands tl)@[Equal]
                  | "lessThan" -> (convert_to_commands tl)@[LessThan]
                  | "bind" -> (convert_to_commands tl)@[Bind]
                  | "if" -> (convert_to_commands tl)@[If]
                  | "let" -> (convert_to_commands tl)@[Let]
                  | "end" -> (convert_to_commands tl)@[End]
                  | _ -> (convert_to_commands tl)  )
   | _ :: [] -> []


(* gonna bank on the fact that its always gonna be in this order
push salem 
push 1
bind 
push sigil 
push 3
bind 
so gonna loop through the command list independent of executeCommand() to get the map 
reverseCmdList reverses the command list(reversed already) so it puts it back to original order *)
let rec reverseCmdList(command_lst : command list):command list =
   match command_lst with
   | [] -> []
   | hd :: tl -> (reverseCmdList tl)@[hd]   
(* command_lst should be in this order (Push(salem),Push(1),Bind) *)
let rec createJankyAssMap(command_lst : command list)  : (string * string) list=
   (match command_lst with
   | [] -> []
   | pname::pval::bd::tl -> (match (bd,pname,pval) with
                              | (Bind,Push(n),Push(v))   ->(   if(is_name n && (compare (push_top v) ":error:") != 0  ) then                               
                                                                  (createJankyAssMap tl)@[(n, push_top v)] 
                                                               else createJankyAssMap(pval::bd::tl)   )
                              | _ ->    createJankyAssMap (pval::bd::tl) )
                              | _::_::[] -> []
   | _::_::[] -> []
   | _ :: [] -> []) 

(* 
checks the bindings ie if [(a,b);(b,8)] then it will change it to [(a,8);(b,8)]  
[("b", "a")] is a no-no cant bind a name to another name that isnt bound to a value
*)
let rec checkJankyMap(mappy : (string * string) list)  : (string * string) list=
   match mappy with
   | [] -> []
   | hd::tl -> (match hd with 
               | (k, v) ->( if(mapContains (v, tl)) then 
                                 (k, getMapValue (v,tl) )::(checkJankyMap tl)
                            else if (is_name v) then
                                 (checkJankyMap tl)
                            else
                                 (k, v)::(checkJankyMap tl) )
               | _ -> [("broked","adsf")] )



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
   let cmd_list = convert_to_commands listy in
   let bindMap = createJankyAssMap (reverseCmdList cmd_list) in
   let bindMap = checkJankyMap bindMap in

   let println_op(st : string list): string list =
      (match st with
      |[] -> [":error:"]
      |hd::tl -> if (contains hd "-s")  then (file_write_string (removeStringMarker hd); tl) else ":error:"::hd::tl )    in
(* 
   executes the commands in the let end blocks  
   if IF command is detected then stop the whole thing*)
   let rec executeCmdLetEnd( (command_lst : command list) ,(mappy:(string * string) list) ): string list=
      (match command_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Push(x)   -> (push_top x)::(executeCmdLetEnd (tl,mappy) )    
                     | Pop       -> pop_top       (executeCmdLetEnd (tl,mappy) )      
                     | Add       -> add_op        (executeCmdLetEnd (tl,mappy) , mappy)   
                     | Sub       -> sub_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Rem       -> rem_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Mul       -> mul_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Div       -> div_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Neg       -> neg_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Swap      -> swap_op       (executeCmdLetEnd (tl,mappy) ) 
                     | ToString  -> toString_op   (executeCmdLetEnd (tl,mappy) ) 
                     | Println   -> println_op    (executeCmdLetEnd (tl,mappy) ) 
                     | Cat       -> cat_op        (executeCmdLetEnd (tl,mappy) ) 
                     | And       -> and_op        (executeCmdLetEnd (tl,mappy) , mappy)
                     | Or        -> or_op         (executeCmdLetEnd (tl,mappy) , mappy)
                     | Not       -> not_op        (executeCmdLetEnd (tl,mappy) , mappy)
                     | Equal     -> equal_op      (executeCmdLetEnd (tl,mappy) ) 
                     | LessThan  -> lessThan_op   (executeCmdLetEnd (tl,mappy) ) 
                     | Bind      -> bind_op_stack (  executeCmdLetEnd (tl, (popMap mappy) ) , mappy     )
                     | If        -> if_op         (executeCmdLetEnd (tl,mappy) ) 
                     | Let       -> []
                     | Quit      ->                executeCmdLetEnd (tl,mappy)  
                     | _ ->                        executeCmdLetEnd (tl,mappy) )
      | _ :: [] -> []) in

   let get_head_of_let_end_stack (str_lst : string list): string =
   match str_lst with
   | [] -> ""
   | hd::tl -> hd in

   (* grabs the rest of the commands past the let end block *)
   let rec pastLet (cmd_lst : command list): command list =
      match cmd_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Let  -> tl
                     | _ ->  pastLet tl)  in


   (* takes in  (executeCmdLetEnd (tl,mappy)) which is a  string list *)
   let end_op (str_lst : string list): string  = get_head_of_let_end_stack str_lst in
        

   let rec executeCommand( (command_lst : command list) ,(mappy:(string * string) list) ): string list=
      (match command_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Push(x)   -> (push_top x)::(executeCommand (tl,mappy) )    
                     | Pop       -> pop_top       (executeCommand (tl,mappy) )      
                     | Add       -> add_op        (executeCommand (tl,mappy) , mappy)   
                     | Sub       -> sub_op        (executeCommand (tl,mappy) , mappy) 
                     | Rem       -> rem_op        (executeCommand (tl,mappy) , mappy) 
                     | Mul       -> mul_op        (executeCommand (tl,mappy) , mappy) 
                     | Div       -> div_op        (executeCommand (tl,mappy) , mappy) 
                     | Neg       -> neg_op        (executeCommand (tl,mappy) , mappy) 
                     | Swap      -> swap_op       (executeCommand (tl,mappy) ) 
                     | ToString  -> toString_op   (executeCommand (tl,mappy) ) 
                     | Println   -> println_op    (executeCommand (tl,mappy) ) 
                     | Cat       -> cat_op        (executeCommand (tl,mappy) ) 
                     | And       -> and_op        (executeCmdLetEnd (tl,mappy) , mappy)
                     | Or        -> or_op         (executeCmdLetEnd (tl,mappy) , mappy)
                     | Not       -> not_op        (executeCmdLetEnd (tl,mappy) , mappy)
                     | Equal     -> equal_op      (executeCommand (tl,mappy) ) 
                     | LessThan  -> lessThan_op   (executeCommand (tl,mappy) ) 
                     | Bind      -> bind_op_stack (  executeCommand (tl, (popMap mappy) ) , mappy )
                     | If        -> if_op         (executeCommand (tl,mappy) ) 
                     | End       ->               (end_op  (executeCmdLetEnd (tl,mappy)) )::(executeCommand (pastLet tl,mappy) ) 
                     | Quit      ->                executeCommand (tl,mappy)  
                     | _ ->                        executeCommand (tl,mappy) )
      | _ :: [] -> []) in


(* ISSUE currently it will pop a pair from the map regardless if bind was sucessful *)

   let stack_list = executeCommand ( cmd_list, bindMap ) in

   let ben (str: string)= () in
   ben "sadfsd"


(* let () = (interpreter ("input1.txt" ,"output1.txt") )
let () = (interpreter ("input2.txt" ,"output2.txt") )
let () = (interpreter ("input3.txt" ,"output3.txt") )
let () = (interpreter ("input4.txt" ,"output4.txt") )
let () = (interpreter ("input5.txt" ,"output5.txt") )
let () = (interpreter ("input6.txt" ,"output6.txt") )
let () = (interpreter ("input7.txt" ,"output7.txt") )
let () = (interpreter ("input8.txt" ,"output8.txt") )
let () = (interpreter ("input9.txt" ,"output9.txt") )
let () = (interpreter ("input10.txt" ,"output10.txt") ) 
let () = (interpreter ("inputjack.txt" ,"outputjack.txt") )   *)


(* let ls_str =["push rock";"push 8";"bind";"push 4";"push rock";"add";"push 7";"push 3";"add";"push 7";"push 8";"sub";"toString";"toString";"toString";"toString"]

let ls_str =["push x";"let";"push 5";"push -1560";"add";"end"]

let ls_str = ["push 1";"let";"push 2";"push 3";"push 4";"end";"push 5"]       should be ["5"; "4"; "1"]

let ls_str = ["let";"push a1";"push 7.2";"bind";"end"]                        should be [":error:"]

let ls_str = ["push b";"push 8";"bind";"push a";"push b";"bind"]              map should be [("a", "8"); ("b", "8")]   stack should be [":unit:"; ":unit:"]


let ls_str = ["push b";"push a";"bind"]                                       map should be empty and stack should be [":error:"]

let ls_str = ["push a";"push 13";"bind";"push name1";"push 3";"bind";"push a";"push name1";"add"; "push 7";"push 8";"lessThan"]       stack: [":true:"; "16"; ":unit:"; ":unit:"]


let ls_str = ["push :unit:";"push name1";"push 5";"bind";"push name3";"push unbound_name";"bind";"push name4";"push :unit:"; "bind";
"push name5";"push \"somehow I manage, over 1 billion sold\""; "bind"; "push name6"; "push :true:";"bind" ] 
















bind



add




 *)
 (* 
 let listy = convert_all_pushes ls_str 
 let cmd_list = convert_to_commands listy 
 let bindMap = createJankyAssMap (reverseCmdList cmd_list) 
 let bindMap = checkJankyMap bindMap
 let stack_list = executeCommand ( cmd_list, bindMap )






let rec executeCmdLetEnd( (command_lst : command list) ,(mappy:(string * string) list) ): string list=
      (match command_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Push(x)   -> (push_top x)::(executeCmdLetEnd (tl,mappy) )    
                     | Pop       -> pop_top       (executeCmdLetEnd (tl,mappy) )      
                     | Add       -> add_op        (executeCmdLetEnd (tl,mappy) , mappy)   
                     | Sub       -> sub_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Rem       -> rem_op        (executeCmdLetEnd (tl,mappy) ) 
                     | Mul       -> mul_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Div       -> div_op        (executeCmdLetEnd (tl,mappy) , mappy) 
                     | Neg       -> neg_op        (executeCmdLetEnd (tl,mappy) ) 
                     | Swap      -> swap_op       (executeCmdLetEnd (tl,mappy) ) 
                     | ToString  -> toString_op   (executeCmdLetEnd (tl,mappy) ) 
                    
                     | Cat       -> cat_op        (executeCmdLetEnd (tl,mappy) ) 
                     | And       -> and_op        (executeCmdLetEnd (tl,mappy) ) 
                     | Or        -> or_op         (executeCmdLetEnd (tl,mappy) ) 
                     | Not       -> not_op        (executeCmdLetEnd (tl,mappy) ) 
                     | Equal     -> equal_op      (executeCmdLetEnd (tl,mappy) ) 
                     | LessThan  -> lessThan_op   (executeCmdLetEnd (tl,mappy) ) 
                     | Bind      -> bind_op_stack (  executeCmdLetEnd (tl, (popMap mappy) ) , mappy     )
                     | If        -> if_op         (executeCmdLetEnd (tl,mappy) ) 
                     | Let       -> []
                     | Quit      ->                executeCmdLetEnd (tl,mappy)  
                     | _ ->                        executeCmdLetEnd (tl,mappy) )
      | _ :: [] -> []) 

   let get_head_of_let_end_stack (str_lst : string list): string =
   match str_lst with
   | [] -> ""
   | hd::tl -> hd 

   (* grabs the rest of the commands past the let end block *)
   let rec pastLet (cmd_lst : command list): command list =
      match cmd_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Let  -> tl
                     | _ ->  pastLet tl)  


   (* takes in  (executeCmdLetEnd (tl,mappy)) which is a  string list *)
   let end_op (str_lst : string list): string  = get_head_of_let_end_stack str_lst 
        

   let rec executeCommand( (command_lst : command list) ,(mappy:(string * string) list) ): string list=
      (match command_lst with
      | [] -> []
      | hd :: tl -> (match hd with
                     | Push(x)   -> (push_top x)::(executeCommand (tl,mappy) )    
                     | Pop       -> pop_top       (executeCommand (tl,mappy) )      
                     | Add       -> add_op        (executeCommand (tl,mappy) , mappy)   
                     | Sub       -> sub_op        (executeCommand (tl,mappy) , mappy) 
                     | Rem       -> rem_op        (executeCommand (tl,mappy) ) 
                     | Mul       -> mul_op        (executeCommand (tl,mappy) , mappy) 
                     | Div       -> div_op        (executeCommand (tl,mappy) , mappy) 
                     | Neg       -> neg_op        (executeCommand (tl,mappy) ) 
                     | Swap      -> swap_op       (executeCommand (tl,mappy) ) 
                     | ToString  -> toString_op   (executeCommand (tl,mappy) ) 
                     
                     | Cat       -> cat_op        (executeCommand (tl,mappy) ) 
                     | And       -> and_op        (executeCommand (tl,mappy) ) 
                     | Or        -> or_op         (executeCommand (tl,mappy) ) 
                     | Not       -> not_op        (executeCommand (tl,mappy) ) 
                     | Equal     -> equal_op      (executeCommand (tl,mappy) ) 
                     | LessThan  -> lessThan_op   (executeCommand (tl,mappy) ) 
                     | Bind      -> bind_op_stack (  executeCommand (tl, (popMap mappy) ) , mappy )
                     | If        -> if_op         (executeCommand (tl,mappy) ) 
                     | End       ->               (end_op  (executeCmdLetEnd (tl,mappy)) )::(executeCommand (pastLet tl,mappy) ) 
                     | Quit      ->                executeCommand (tl,mappy)  
                     | _ ->                        executeCommand (tl,mappy) )
      | _ :: [] -> []) 

 *)