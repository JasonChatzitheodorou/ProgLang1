(*******************************************Exercise 1***********************************************)
fun solve_powers2 n k =
    let	
	fun count_digits arr 30 = 0
	  | count_digits arr i =
	    if (Array.sub(arr, i)) > 0 then 1 + count_digits arr (i + 1)
	    else count_digits arr (i + 1);
	    
	fun decimal_toBinary 0 arr i = arr
	  | decimal_toBinary n arr i = (Array.update(arr, i, n mod 2);
					decimal_toBinary (n div 2) arr (i + 1));

	(*Takes binary representation of number and turns
	  it into lexicographically smallest sum of powers*)
	fun fix_powers k arr digits = 
	    let
		fun loop arr 30 = arr
		  | loop arr j =
		    if Array.sub(arr, j) = 0 then loop arr (j + 1)
		    else (Array.update(arr, j, Array.sub(arr, j) - 1);
			  Array.update(arr, j - 1, Array.sub(arr, j - 1) + 2);
			  arr);
			
	    in
		if digits = k then arr
		else fix_powers k (loop arr 1) (digits + 1)
	    end;

	(*Prints array in list form*)
	fun print_list arr =
	    let
		(*Returns position of the last digit in the array*)
		fun pos_last_digit arr =
		    let
			fun loop arr pos 30 = pos
			  | loop arr pos i =
			    if Array.sub(arr, i) = 0 then loop arr pos (i+1)
			    else loop arr i (i+1);
		    in
			loop arr 0 0
		    end;

		fun print_contents arr pos i =
		    if i = pos then print(Int.toString(Array.sub(arr, i)))
		    else (print(Int.toString(Array.sub(arr, i)) ^ ", ");
					     print_contents arr pos (i + 1));
	    in
		(print("["); print_contents arr (pos_last_digit arr) 0; print("]\n"))
	    end;
	
	fun do_powers2 n k arr =
	    let
		val binary = decimal_toBinary n arr 0;
		val digits = count_digits binary 0;
		
	    in
		if k < digits orelse k > n then print("[]\n")			
		else print_list (fix_powers k binary digits) 
	    end;
		
    in
	do_powers2 n k (Array.array(30, 0))		   
    end;

(*This function was found at https://courses.softlab.ntua.gr/pl1/2013a/Exercises/countries.sml*)
fun parse file =
    let
	fun readInt input = 
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);
	
	val inStream = TextIO.openIn file;

	(*Read t, the number of lines, and a newline*)
	val t = readInt inStream;
	val _ = TextIO.inputLine inStream;
		
	fun readInts 0 acc = rev acc 
	  | readInts i acc = readInts (i - 1) (readInt inStream :: acc);

    in
	(t, readInts (2 * t) [])
    end;

fun powers2 str =
    let
	val (T, N::K::ts) = parse str;

	fun run 1 n k _ = solve_powers2 n k
	  | run t n k (a::b::xs) = (solve_powers2 n k; run (t-1) a b xs)
	  | run t n k _ = (); 
    in
	run T N K ts
    end;


(*******************************************Exercise 2***********************************************)
fun do_coronograph N M l =
    let
	val N = N + 1;
	(*Arrays for arrived, finished, parent because I need fast update of values*)
	val arrived = Array.array(N, 0);
	val finished = Array.array(N, 0);
	val parent = Array.array(N, ~1);
	val adj = Array.array(N, []);

	fun initialise arr v = modify (fn a => v) arr;
	
	fun delete_edge u v =
	    let
		val l_1 = List.filter (fn a => a <> v) (Array.sub(adj, u));
		val l_2 = List.filter (fn a => a <> u) (Array.sub(adj, v));
	    in
		(Array.update(adj, u, l_1); Array.update(adj, v, l_2))
	    end;

	fun add_edge u v =
	    let
		val l_1 = Array.sub(adj, u);
		val l_2 = Array.sub(adj, v);
	    in
		(Array.update(adj, u, v::l_1); Array.update(adj, v, u::l_2))
	    end;

	(*Adds all the given edges to adj*)
	fun create_graph 0 _ = ()
	  | create_graph i (u::v::ts) = (create_graph (i - 1) ts; add_edge u v)
	  | create_graph i _ = ();

	(*Returns number of nodes of tree starting with vertex n*)
	fun DFS_count n =
	    let
		fun loop_neighbours nil = 0
		  | loop_neighbours l =
		    if Array.sub(arrived, hd l) = 0 then (loop_neighbours (tl l) + DFS_count (hd l))
		    else loop_neighbours (tl l); 
	    in
		(Array.update(arrived, n, 1); 1 + loop_neighbours (Array.sub(adj, n)))
	    end;

	(*Fills arrays arrived, finished, parent, and references t, first, last*)
	(*Takes integer as parameter, which is usually the starting node, 1*)
	(*and loops over all nodes of graph*)
	fun do_DFS i t first last = 
	    let
		fun DFS_from_node n t first last =
		    let
			fun loop_neighbours [] = ()
			  | loop_neighbours (h::ts) =
			    if Array.sub(arrived, h) = 0 then (Array.update(parent, h, n);
							       DFS_from_node h t first last;
							       loop_neighbours ts)
			    else if Array.sub(arrived, n) > Array.sub(arrived, h) andalso
				    Array.sub(finished, h) = 0 andalso
				    h <> Array.sub(parent, n) then (first := h; last := n)
			else ();
		    in
			(t := !t + 1;
			 Array.update(arrived, n, !t);
			 loop_neighbours (Array.sub(adj, n));
			 t := !t + 1;
			 Array.update(finished, n, !t))
		    end;
	    in
					     
		if i < N then 			    
		    if Array.sub(arrived, i) = 0 then (DFS_from_node i t first last;
						       do_DFS (i + 1) t first last)
		    else do_DFS (i + 1) t first last
		else ()
	    end;

	fun find_cycle _ = ();
    in
	(create_graph M l; delete_edge 1 2; DFS_count 4)
    end;

fun test _ = do_coronograph 5 4 [1, 2, 2, 3, 1, 3, 5, 4];
		
fun coronograph str =
    let
	fun loop_t 0 _ _ _ = ()
	  | loop_t T N M l = ();

	fun loop_m 0 _ _ _ = ()
	  | loop_m M _ _ _  = (); 
    in
	()
    end;
	
