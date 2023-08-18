type 'a node =
    | One of 'a 
    | Many of 'a node list;;

(* Solution to problem 01 *)

let rec last (arr : 'a list) : 'a option = 
	match arr with
	| []  	 -> None
	| [a] 	 -> Some a
	| _ :: t -> last t

(* Solution to problem 02 *)

let rec last_two (arr : 'a list) : ('a * 'a) option =
	match arr with
	| [a; b] -> Some (a, b)
	| _ :: t -> last_two t
	| _ 	 -> None

(* Solution to problem 03 *)


let at (k : int) (arr : 'a list) : 'a option = 
	let rec at' (arr1 : 'a list) (counter : int) : 'a option = 
		match arr1 with
		| []     -> None
		| a :: t -> if counter < k then at' (t) (counter + 1) else Some a

	in at' arr 1

(* Solution to problem 04 *)

let length (arr : 'a list) : int =
    let rec length' (arr1 : 'a list) (counter : int) : int = 
        match arr1 with
        | []     	-> counter
        | _ :: t    -> length' (t) (counter + 1)

    in length' arr 0

(* Solution to problem 05 *)

let rev (arr : 'a list) : 'a list =
	let rec rev' (arr1 : 'a list) (output : 'a list) = 
		match arr1 with
		| [] -> output
		| h :: t -> rev' t (h :: output) 

	in rev' arr []

(* Solution to problem 06 *)

let is_palindrome (arr : 'a list) : bool = 
	arr = rev arr

(* Solution to problem 07 *)

let rec flatten (arr : ('a node) list) : 'a list = 
	let rec flatten' (arr1 : ('a node) list) (output : 'a list) : 'a list = 
		match arr1 with
		| [] 	 -> output
		| h :: t -> 
			match h with
			| One m  -> flatten' t (output @ [m])
			| Many o -> flatten' t (output @ (flatten o))

	in flatten' arr []

(* Solution to problem 08 *)

let compress (arr : 'a list) : 'a list = 
	let rec compress' (arr1 : 'a list) (current : 'a) (output : 'a list): 'a list = 
		match arr1 with
		| [] 	 -> output
		| h :: t -> (compress' t 
							(if h = current then current else h)
							(if h = current then output else (output @ [h]))) 

	in compress' arr (List.hd arr) [List.hd arr]