open Search;;

class ['a] bf = 
    object (self)
        val mutable ls = []

        method push (x : 'a) = ls <- (ls @ [x])

        method pop : 'a option = match ls with
            | [] -> None
            | x::ls' -> ls <- ls'; Some x
    end;;

class gridstate xinit yinit =
    object (self)
        val x : int = xinit
        val y : int = yinit

        method getx = x
        method gety = y

        method equals (other : gridstate) = 
            (x == other#getx) && (y == other#gety)
    end;;

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

        method equals (other : gridnode) =
            s#equals other#state

        method copy =
            new gridnode s p a

        method applymove (xd, yd) =
            new gridnode (new gridstate (s#getx + xd) (s#gety + yd)) (Some self#copy) (Some (xd, yd))

        method expand = 
            Printf.printf "Expanding Node (%d, %d)" s#getx s#gety; print_newline ();
            [self#applymove (1, 0);
             self#applymove (0, 1);
             self#applymove (-1, 0);
             self#applymove (0, -1)]
    end;;

(*let bfs = search (new bf);;*)
let createnode x y = new gridnode (new gridstate x y) None None;;
