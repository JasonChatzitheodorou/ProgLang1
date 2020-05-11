local 
    (*Returns 2d Array with the input*)
    fun makeConfig l = Array2.fromList l;
    (*---------------------------------------*)
    (*The moves are sorted in lexicographical order*)
    val valid_moves = ["D", "L", "R", "U"];
    (*---------------------------------------*)
    (*Prints 2D-Array of characters*)
    fun printArray2 arr =
	let
	    fun do_printArray2 arr i =
		let
		    val row = Array2.row(arr, i);
		    val max_row = Array2.nRows arr;
		    fun printRow r = Vector.app (fn x => print(Char.toString(x))) r
		in
		    if i = (max_row - 1) then (printRow row; print("\n"))
		    else (printRow row; print("\n");
			  do_printArray2 arr (i+1))
		end;
	in
	    do_printArray2 arr 0
	end;
    (*---------------------------------------*)
    fun parse file =
	let
	    val inStream = TextIO.openIn file;
	    fun readInput inStream =
		case TextIO.inputLine inStream of
		    SOME line => line :: (readInput inStream)
		  | NONE => [];
	in
	    readInput inStream
	end;
    (*---------------------------------------*)
    fun do_stayhome N M config A W S (i_T, j_T) =
	let
	    (*Initialise S*)
	    val S_positions = Array2.array(N, M, #".");
	    val (i_S, j_S) = hd S;
	    val _ = Array2.update(S_positions, i_S, j_S, #"S");

	    (*Initialise W*)
	    val W_positions = Array2.array(N, M, ~1);
	    val (i_W, j_W) = hd W;
	    val _ = Array2.update(W_positions, i_W, j_W, 0);

	    (*Initialise parent*)
	    val parent = Array2.array(N, M, (~1, ~1));
	    
	    val min_moves = ref 0;
	    val airport_infected = ref ~1;
	    (*----------------------------------------*)
	    fun newCoords (i, j) move =
		case move of
		    "D" => (i+1, j)
		  | "U" => (i-1, j)
		  | "L" => (i, j-1)
		  | "R" => (i, j+1)
		  | _ => (0, 0);
	    (*----------------------------------------*)
	    (*Function that checks whether the position of 
	  a move is valid. 
	  It must be within bounds and not walk on
	  S's or W's*)
	    fun checkPosition (i, j) =
		(i >= 0 andalso i < N andalso
		 j >= 0 andalso j < M andalso
		 Array2.sub(config, i, j) <> #"X" andalso
		 Array2.sub(W_positions, i, j) = ~1 andalso
		 Array2.sub(S_positions, i, j) = #".");
	    (*----------------------------------------*)
	    (*Function that spreads S in lexicographic order
          and returns list of their new positions. 
          Also fills parent*)
	    fun move_S [] = []
	      | move_S (curr::rest) =
		let
		    val spread = List.map (newCoords curr) valid_moves;
		    val valid_spread = List.filter checkPosition spread;
		    val _ =
			List.app
			    (fn (i, j) => (Array2.update(S_positions, i, j, #"S");
					   Array2.update(parent, i, j, curr)))
			    valid_spread
		in
		    valid_spread @ (move_S rest)
		end;
	    (*----------------------------------------*)
	    (*Function that spreads each W, two time units
	  after it was last spread and returns list of
	  their new positions. ---!AIRPORTS!---*)
	    fun move_W _ [] = []
	      | move_W time ((i, j)::rest) = 
		let
		    val spread_flag = time - Array2.sub(W_positions, i, j) = 2;
		    val spread = List.map (newCoords (i, j)) valid_moves;
		    val valid_spread = if spread_flag
				       then List.filter checkPosition spread	 
				       else [(i, j)];
		    
		    fun update_W (i, j) = Array2.update(W_positions, i, j, time);
		    fun isAirportInfected (i, j) = Array2.sub(config, i, j) = #"A";
		in
		    ((*print(Int.toString(time) ^ ", " ^
		       Int.toString(!airport_infected) ^ ", " ^
		       Int.toString(i) ^ ", " ^
		       Int.toString(j) ^ "\n");*)
		      if spread_flag
		      then (if !airport_infected = ~1
			    then
				case List.find isAirportInfected valid_spread of
				    NONE => ()
				  | SOME(_) => airport_infected := time
			    else ();
			    List.app update_W valid_spread;
			    valid_spread @ (move_W time rest))
		      else valid_spread @ (move_W time rest))
		end;
	    (*----------------------------------------*)
	    (*If S reaches T, the minimum moves are equal to
	  time, else if W blocks S set the minimum moves to ~1
	  else continue*)
	    fun spread time S W =
		let
		    val teleportA_flag = !airport_infected <> ~1 andalso
					 time - !airport_infected = 5;
		    val newW = if teleportA_flag
			       then (List.app (fn (i, j) => Array2.update(W_positions, i, j, time)) A;
				     move_W time W @ A)
			       else move_W time W;
		    val newS = move_S S;
		in
		    if null newS
		    then min_moves := 0
		    else if Array2.sub(S_positions, i_T, j_T) = #"S"
		    then min_moves := time
		    else spread (time + 1) newS newW
		end;
	    (*----------------------------------------*)
	    (*Traverses parent from a visited node 
          to the first S*)
	    fun backtrack (i, j) =
		let
		    val p = Array2.sub(parent, i, j);

		    (*Finds which direction parent moved
		  to reach curr*)
		    fun findMove par curr =
			List.find (fn x => newCoords par x = curr) valid_moves;
		in
		    if p = (~1, ~1) then ""
		    else case findMove p (i, j) of
			     NONE => ""
			   | SOME(m) => (backtrack p) ^ m
		end;
	    (*----------------------------------------*)
	in
	    (spread 1 S W;
	     if !min_moves <> 0
	     then print(Int.toString(!min_moves) ^ "\n" ^
			backtrack(i_T, j_T) ^ "\n")
	     else print("IMPOSSIBLE\n")
	    (*printArray2 S_positions*))
	end;
in
(*Creates 2D-Array config and calls do_stayhome*)
fun stayhome file =
    let	
	val input_list = parse file;
	val list_with_enter = List.map String.explode input_list;
	val list = map (List.filter (fn x => x <> #"\n")) list_with_enter; 
	val config = Array2.fromList list;
	val (N, M) = Array2.dimensions config;
	(*--------------------------------------------------------------------------------------*)
	(*Returns list of locations of character ch*)
	fun findCharacters (~1, ~1) _ = []
	  | findCharacters (i, j) ch =
	    let
		val (x : char) = Array2.sub(config, i, j);
		val next_position =
		    if i+1 < N then (i+1, j)
		    else if j + 1 < M then (0, j+1)
		    else (~1, ~1);
	    in
		if x = ch then ((i, j)::(findCharacters next_position ch))
		else (findCharacters next_position ch)
	    end;
	(*--------------------------------------------------------------------------------------*)
	val S = findCharacters (0, 0) (#"S");
	val W = findCharacters (0, 0) (#"W");
	val A = findCharacters (0, 0) (#"A");
	val T = case findCharacters (0, 0) (#"T") of
		    [] => (~1, ~1)
		  | x::_ => x; 
    in
	do_stayhome N M config A W S T
    end;
end;
