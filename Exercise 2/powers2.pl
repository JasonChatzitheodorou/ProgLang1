/*Returns list of digits in reverse order, from LSB to MSB,
  and the number of digits of the binary number*/
decimal_toBinary(0, Answer, Digits) :-
    Answer = [],
    Digits = 0.
decimal_toBinary(N, Answer, Digits) :-
    N > 0,
    Current is N mod 2,
    N_new is N // 2,
    decimal_toBinary(N_new, Answer_new, Digits_new),
    Answer = [Current | Answer_new],
    (Current =:= 1 -> Digits is (1 + Digits_new)
    ;Digits is Digits_new).

/*Loops through list, every time increases digits by one*/
do_fix_powers(_, [], _, _).
do_fix_powers(K, [Prev, Curr | Tail], L_ans) :-
    (Curr > 0 ->
	 Curr_new is Curr - 1,
	 Prev_new is Prev + 2,
	 L_ans = [Prev_new, Curr_new | Tail]
    ;do_fix_powers(K, [Curr | Tail], L_new),
     L_ans = [Prev | L_new]).

/*Answer is the compact form of the lexicographically least sum*/
fix_powers(K, L, K, Answer) :-
    Answer = L.
fix_powers(K, L, Digits, Answer) :-
    Digits < K,
    do_fix_powers(K, L, L_new),
    Digits_new is Digits + 1,
    fix_powers(K, L_new, Digits_new, Answer).

remove_starting_zeros([], Answer) :-
    Answer = [].
remove_starting_zeros([Head | Tails], Answer) :-
    (Head \= 0 ->
	 Answer = [Head | Tails]
    ;remove_starting_zeros(Tails, Answer)).

do_powers2(N, K, Answer) :-
    decimal_toBinary(N, Binary, Digits),
    (N >= K, K >= Digits ->
	 fix_powers(K, Binary, Digits, Temp),
	 reverse(Temp, Temp_rev),
	 remove_starting_zeros(Temp_rev, Temp_answer),
	 reverse(Temp_answer, Answer)
    ;Answer = []).

loop_powers2([], _, Answers) :-
    Answers = [].
loop_powers2([[N, K] | Tails], Stream, Answers) :-
    do_powers2(N, K, Curr_answer), 
    loop_powers2(Tails, Stream, Rest_answers),
    Answers = [Curr_answer | Rest_answers].

powers2(File, Answers) :-
    open(File, read, Stream),
    read_line(Stream, T),
    read_input(T, Stream, L),
    !,
    loop_powers2(L, Stream, Answers).
    
read_input(0, _, L) :- L = [].
read_input(T, Stream, L) :-
    read_line(Stream, [N, K]),
    T_new is T - 1,
    read_input(T_new, Stream, L_new),
    L = [[N, K] | L_new].

/*The following function was found in
  https://courses.softlab.ntua.gr/pl1/2019a/Exercises/read_colors_SWI.pl*/
read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

