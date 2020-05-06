local
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
			else (print(Int.toString(Array.sub(arr, i)) ^ ",");
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
in
fun powers2 str =
    let
	val (T, s) = parse str;
	val N = hd s;
	val K = hd (tl s);
	val ts = tl (tl s);

	fun run 1 n k _ = solve_powers2 n k
	  | run t n k (a::b::xs) = (solve_powers2 n k; run (t-1) a b xs)
	  | run t n k _ = (); 
    in
	run T N K ts
    end;
end;
