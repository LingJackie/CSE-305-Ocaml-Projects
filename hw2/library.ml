let welcome = "Welcome to the Oakland, California Municipal Library (OCaML)"

(* These types are defined for you. You should not modify them *)
type catalog_item = Book of string * string | Movie of string * int | CD of string * string | Computer
type checkout_entry = Item of catalog_item | New of checkout_entry | Pair of checkout_entry * checkout_entry
type checkout = Checkout of checkout_entry * int option

(* Examples *)
(* These are some examples of checkout_item. You should test locally with these before submitting *)
let i0 = Book ("Types and Programming Languages", "Benjamin Pierce")
let i1 = Movie ("The Imitation Game", 2014)
let i2 = Computer

(* These are some examples of checkout_entry. You should test locally with these before submitting *)
let e0 = Item i0
let e1 = Item i1
let e2 = Item i2

let e3 = Item (CD ("Songs to Test By", "Aperture Science Psychoacoustic Laboratories"))
let e4 = New (Item (Book ("UNIX: A History and a Memoir", "Brian W. Kernighan")))

let e5 = Pair (
    Item (Movie ("WarGames", 1983)),
    Item (Movie ("Sneakers", 1992))
)

let e6 = Pair (
    Pair (
        Item (Book ("The Unix Programming Environment", "Brian W. Kernighan and Rob Pike")), 
        Item (Book ("The C Programming Language", "Brian Kernighan and Dennis Ritchie"))
    ),
    Item (Book ("The AWK Programming Language", "Alfred V. Aho, Brian W. Kernighan, and Peter J. Weinberger"))
)

(* This is an exmaple of a checkout list. You should test locally with it before submitting *)
let checked_out = [Checkout (e1, Some 2); Checkout (e2, None); Checkout (e4, Some 1); Checkout (e5, Some 2)]

(* The following functions you must implement *)

(* Display item as string (title, author) -> String.concat "by" [title, author]*)
let string_of_item (i : catalog_item) : string = 
    match i with
    | Book (title, author) -> String.concat " by " [title; author]
    | Movie (title, year)-> String.concat " " [title; String.concat (string_of_int year) ["("; ")"] ]
    | CD (album, artist) -> String.concat " by " [album; artist]
    | Computer -> "Public Computer"


(* Display entry as string *)
let rec string_of_entry (e : checkout_entry) : string = 
    match e with
    | Item (catalogIt)-> string_of_item(catalogIt)
    | New (checkEnt)-> String.concat " " ["(NEW)"; string_of_entry checkEnt]  
    | Pair (entry1, entry2) -> String.concat " and " [string_of_entry entry1; string_of_entry entry2]
   

    (*Printf.sprintf "%s\n" string_of_item(i0)*)

(* Return the daily fine for an overdue item *)
let daily_fine_helper (i : catalog_item) : float = 
    match i with
    | Book (title, author) -> 0.25
    | Movie (title, year) -> 0.5
    | CD (album, artist)-> 0.5
    | Computer -> 0.0
let rec daily_fine (entry: checkout_entry) : float = 
    match entry with
    | Item (catalogIt)-> daily_fine_helper (catalogIt)
    | New (checkEnt)-> 2.0 *. daily_fine checkEnt
    | Pair (entry1, entry2) -> daily_fine entry1 +. daily_fine entry2

(* Given a list of items and days overdue, compute the total fine *)
let total_fine_helper (s : int option) : float = 
    match s with
    | None -> 0.0
    | Some(stuff) -> float_of_int (stuff)
    


let rec total_fine (l : checkout list) : float = 
    match l with
    | [] -> 0.0
    |  Checkout (check_out_entry, int_op ) ::tl ->  daily_fine check_out_entry *. (total_fine_helper int_op) +. total_fine tl 
    
    (*| (ch_out(ch_out_entr), int_op)::tl -> ((daily_fine ch_out_entr) *. (total_fine_helper int_op)) +. total_fine tl *)

