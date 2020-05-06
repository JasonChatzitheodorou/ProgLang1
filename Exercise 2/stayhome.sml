(*Returns list of lists with characters *)
fun read_input file = ();

(*Returns 2d Array with the input*)
fun makeConfig l = Array2.fromList l;

fun sizeSortStrings s =
	    ListMergeSort.sort
		(fn (s : string, t) =>
		    (String.size s) > (String.size t)) s;

fun lexSortStrings s = ListMergeSort.sort (fn (s : string, t) => s > t) s;
	
val valid_moves = ["D", "U", "L", "R"];

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

fun do_stayhome N M config infected position =
    let
	val visited = Array2.array(N, M, false);

	val (i, j) = position;
	
	fun newCoords (i, j) move =
	    case move of
		"D" => (i+1, j)
	      | "U" => (i-1, j)
	      | "L" => (i, j-1)
	      | "R" => (i, j+1)
	      | _ => (0, 0);
	
	(*Returns a string with the moves made.
	Changes infected positions when time > 0 
	and time % 2 = 0.*)
	fun nextMoment time config visited infected prev_position move =
	    let	
		(*Is true is position is withing bounds,
		  not X and not W*)
		fun checkPosition (i, j) =	
			(i >= 0 andalso i < N andalso
			 j >= 0 andalso j < M andalso
			 Array2.sub(config, i, j) <> #"X" andalso
			 Array2.sub(config, i, j) <> #"W");

		(*Finds new infected positions and writes them on config
		  No need to save all infected positions on the list, 
		  once a position infects those around it, it won't infect
		  again*)
		fun moveCorona [] = []
		  | moveCorona (infected_spot::ts) = 
		    let
			fun changeConfig (i, j) = Array2.update(config, i, j, #"W");
			    (*(if Array2.sub(config, x) = "A"
			     then () (*....FLAG AIRPORT.....*)
			     else  ();
			     Array2.update(config, x, "W"));*)
									 
			val possible_positions = List.map (newCoords infected_spot) valid_moves;
			val valid_positions = List.filter (checkPosition) possible_positions;
			(*val a = List.app (fn (x, y) => print(Int.toString(x) ^ " " ^ Int.toString(y) ^ "\n"))
					 valid_positions;*)
		    in
			(List.app changeConfig valid_positions;
			 valid_positions @ (moveCorona ts))
		    end;

		(*Current position after move*)
		val new_position = newCoords prev_position move;
		val (i_new, j_new) = new_position;
		
		(*Current infected position after move*)
		val new_infected = if time mod 2 = 0 andalso time > 0
				   then moveCorona infected
				   else infected;

		val a = print("We're here:" ^ Int.toString(i_new) ^ Int.toString(j_new) ^ "\n");	
		(*If new position is ok, it is true, else false*)
		val flag = if (checkPosition new_position) andalso
			      (Array2.sub(visited, i_new, j_new) = false)
				then (Array2.update(visited, i_new, j_new, true); true)
			   else false;
		
		val new_time = time + 1;
		fun movePersonTo c = nextMoment new_time config visited new_infected new_position c;
		fun findSolutions () = List.map movePersonTo valid_moves; 
		fun sizeSortedSolutions () = sizeSortStrings (findSolutions());
		fun solution () =
		    case (List.find (fn s => s <> "") (sizeSortedSolutions())) of
			NONE => ""
		      | SOME(x) => x; 
	    in
		(*If he has reached home, return the move that brought him there,
		  else check whether his position is OK and return all the moves made*)
		(if flag andalso Array2.sub(config, i_new, j_new) = #"T" then move
		 else if flag then
		     case (solution()) of
			 "" => ""
		       | s => move ^ s
		 else "")
	    end;

	fun movePersonTo c = nextMoment 1 config visited infected position c;
	fun findSolutions () = List.map movePersonTo valid_moves; 
	fun sizeSortedSolutions () = sizeSortStrings (findSolutions());
	val answer =
	    case List.find (fn s => s <> "") sizeSortedSolutions() of
		NONE => ""
	      | SOME(x) => x;
    in
	 if answer = "" then print("IMPOSSIBLE\n")
	 else print("Made it " ^
		    Int.toString (size answer) ^ "\n" ^
		    answer ^ "\n")
	    
    end;



(*Creates 2D-Array config*)
fun stayhome file =
    let	
	val input_list = parse file;
	val list_with_enter = List.map String.explode input_list;
	val list = map (List.filter (fn x => x <> #"\n")) list_with_enter; 
	val config = Array2.fromList list;
	val (N, M) = Array2.dimensions config;

	(*Finds location of character ch*)
	fun findCharacters (~1, ~1) _ = (~1, ~1)
	  | findCharacters (i, j) ch =
	    let
		val (x : char) = Array2.sub(config, i, j);
		val next_position =
		    if i+1 < N then (i+1, j)
		    else if j+1 < M then (0, j+1)
		    else (~1, ~1);
	    in
		if x = ch then (i, j)
		else (findCharacters next_position ch)
	    end;

	val position = findCharacters (0, 0) (#"S");
	val infected = [findCharacters (0, 0) (#"W")];
    in
	do_stayhome N M config infected position
    end;

fun test() = stayhome "test_stayhome.txt";
