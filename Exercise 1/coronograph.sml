local 
    fun do_coronograph N M l =
	let
	    val N = N + 1;
	    (*Arrays for arrived, finished, parent because I need fast access and update of values*)
	    val arrived = Array.array(N, 0);
	    val finished = Array.array(N, 0);
	    val parent = Array.array(N, ~1);
	    val adj = Array.array(N, []);

	    fun initialise arr v = Array.modify (fn a => v) arr;
	    
	    fun remove_edge u v =
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

	    (*Takes list of nodes, splits them in two's starting from the head,*)
	    (*and adds all the given edges to adj*)
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
	    (*and loops over all nodes of tree starting with node 1*)
	    fun do_DFS t first last =
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
					h <> Array.sub(parent, n) then (first := h; last := n; loop_neighbours ts)
				else loop_neighbours ts;
			in
			    (t := !t + 1;
			     Array.update(arrived, n, !t);
			     loop_neighbours (Array.sub(adj, n));
			     t := !t + 1;
			     Array.update(finished, n, !t))
			end;
		in
		    DFS_from_node 1 t first last
		end;

	    (*Sets visited[0] = 1 to check whether all other nodes have been visited*)
	    fun check_connectivity () = (Array.update(arrived, 0, 1);
					not (Array.exists (fn x => x = 0) arrived));
	    
	    (*Initialises the 3 arrays and returns list of nodes in a cycle, using array parent*)
	    fun find_cycle () =
		let
		    val first = ref ~1;
		    val last = ref ~1;
		    val time = ref 0;

		    (*first and last are references to the first and last nodes of the cycle*)
		    fun list_cycle ~1 = []
		      | list_cycle index =
			if index <> Array.sub(parent, !first) then index::(list_cycle (Array.sub(parent, index)))
			else [];
		in
		    (initialise arrived 0; initialise finished 0; initialise parent (~1);
		     do_DFS time first last; list_cycle (!last))
		end;

	    fun solve () =
		let
		    val cycle = find_cycle ();
		    val connected = check_connectivity ();

		    fun success l =
			let
			    fun remove_edges_of_cycle [] = ()
			      | remove_edges_of_cycle [u] = () 
			      | remove_edges_of_cycle (u::ts) = (remove_edge u (hd ts);
								 remove_edges_of_cycle ts)

			    fun print_list [] = ()
			      | print_list [h] = print(Int.toString(h) ^ "\n") 
			      | print_list (h::ts) = (print(Int.toString(h) ^ " "); print_list ts); 

			    (*Curried function that sorts a list*)
			    val sort_list = ListMergeSort.sort (fn(x, y) => x > y);
			in
			    (remove_edges_of_cycle l;
			     initialise arrived 0;
			     print("CORONA " ^ Int.toString(length l) ^ "\n");
			     print_list (sort_list (map DFS_count l)))
			end;
		in
		    (*Only if the nodes are equal to the edges can we possibly have a unique cycle
		      but N has been increased by one already so we check N != M + 1*)
		    (*If nodes are equal to edges and we have a connected graph then the cycle is unique*)
		    if (N <> M+1) orelse (null cycle) orelse (not connected) then print("NO CORONA\n")
		    else (remove_edge (hd cycle) (List.last cycle); success cycle)
		end;
	    
	in
	    (create_graph M l; solve())
	end;
in
(*Code for input from file was found at https://courses.softlab.ntua.gr/pl1/2013a/Exercises/countries.sml*)		
fun coronograph str =
    let
	val inStream = TextIO.openIn str;

	fun readInt input = 
	    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

	fun readInts 0 acc = rev acc
	  | readInts i acc = readInts (i - 1) (readInt inStream :: acc);

	val T = readInt inStream;
	(*val _ = TextIO.inputLine inStream;*)
	
	fun loop_t 0 = ()
	  | loop_t t =
	    let	
		val s = readInts 2 []; 
		val N = hd s;
		val M = hd (tl s);
		val l = readInts (2 * M) [];
	    in
		(do_coronograph N M l; loop_t (t - 1))
	    end;		 
    in
	loop_t T
    end;
end;
