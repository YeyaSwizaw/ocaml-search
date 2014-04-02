open Search;;

(* A breadth first search frontier - uses a queue *)
class ['a] bf = 
    object (self)
        val mutable ls = []

        (* Appends an item to the queue *)
        method push (x : 'a) = ls <- (ls @ [x])

        (* Returns Some (top of queue) unless the queue is empty, in which case
         * returns None. *)
        method pop : 'a option = match ls with
            | [] -> None
            | x::ls' -> ls <- ls'; Some x
    end;;

(* A very basic 'grid' location. Basically just an (x, y) coordinate *)
class gridstate xinit yinit =
    object (self)
        val x : int = xinit
        val y : int = yinit

        method getx = x
        method gety = y

        method equals (other : gridstate) = 
            (x == other#getx) && (y == other#gety)
    end;;

(* A search node storing a grid state *)
class gridnode st pa ac =
    object (self)
        val s : gridstate = st
        val p : gridnode option = pa
        val a : (int * int) option = ac

        method state = s
        method parent = p
        method action = a

        method print = match a with
            | None -> Printf.printf "(%d, %d)" s#getx s#gety
            | Some (xd, yd) -> Printf.printf "(%d, %d) -> (%d, %d)" xd yd s#getx s#gety

        (* Returns true if the stored state is the same as the provided node's state *)
        method equals (other : gridnode) =
            s#equals other#state

        (* Creates a new node identical to this one *)
        method copy =
            new gridnode s p a

        (* Creates a new node - a copy of this one with the provided move applied *)
        method applymove (xd, yd) =
            new gridnode (new gridstate (s#getx + xd) (s#gety + yd)) (Some self#copy) (Some (xd, yd))

        (* Returns a list of all possible states after this one *)
        method expand = 
            Printf.printf "Expanding Node (%d, %d)" s#getx s#gety; print_newline ();
            [self#applymove (1, 0);
             self#applymove (0, 1);
             self#applymove (-1, 0);
             self#applymove (0, -1)]
    end;;

let createnode x y = new gridnode (new gridstate x y) None None;;
