(* Takes an option type and returns either None or Some (result of function 
 * applied to value) *)
let mapmaybe f x = match x with 
    | None -> None
    | Some x' -> f x';;

(* Calls a function for each item in a list *)
let rec foreach f ls = match ls with
    | [] -> ()
    | x::ls' -> f x; foreach f ls';;

(* Performs search from the start node to the end node, using the provided frontier.
 * The frontier implementation decides the search strategy, for example a queue 
 * is Breadth First Search. *)
let search frontier start goal =
    let rec mem set x = match set with
        | [] -> false
        | x'::set' -> x#equals x' || mem set' x
    in

    let rec add set x = 
        if mem set x then 
            set 
        else 
            x::set 
    in

    let pushlist explored states = 
        foreach (fun s -> if mem explored s then () else frontier#push s) states
    in

    let rec getpath node = let parent = node#parent in match parent with
        | None -> [node]
        | Some p' -> getpath p' @ [node]
    in

    let rec checknode explored node = 
        if node#equals goal then
            Some (getpath node)
        else
            let explored' = add explored node in
            pushlist explored' node#expand;
            search' explored'

    and search' explored = 
        mapmaybe (checknode explored) frontier#pop 
    in

    frontier#push start; 
    search' [];;

