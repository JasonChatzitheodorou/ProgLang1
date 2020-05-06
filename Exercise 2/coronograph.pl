/*
Library ugraph is preloaded in SWI Prolog
Documentation was found here: 
https://www.swi-prolog.org/pldoc/man?section=ugraphs
*/

arrived/2.
finished/2.

:- dynamic
       parent/2,
       visited/1,
       backedge/2.

/*--------------------------------------
Create and maintain Graph
--------------------------------------*/
create_vertices(0, Vertices) :-
    Vertices = [].
create_vertices(N, Vertices) :-
    N > 0,
    Nnew is N - 1,
    create_vertices(Nnew, Rest_vertices),
    Vertices = [N | Rest_vertices].

create_graph(N, G) :-
    create_vertices(N, Vertices),
    vertices_edges_to_ugraph(Vertices, [], G).

add_undirected_edge(Graph, U, V, G) :-
    Edge1 = U-V,
    Edge2 = V-U,
    add_edges(Graph, [Edge1, Edge2], G).

delete_undirected_edge(Graph, U, V, G) :-
    Edge1 = U-V,
    Edge2 = V-U,
    del_edges(Graph, [Edge1, Edge2], G).

/*-------------------------------------
Count nodes of tree starting with Node
-------------------------------------*/
dfs_count(Node, Graph, Count) :-
    reachable(Node, Graph, Tree_nodes),
    length(Tree_nodes, Count).

/*---------------------------------------
Classic DFS, finds backedges.
Is true if cycle is found, false
otherwise.
---------------------------------------*/
debugger(Text, A, B) :-
    write(Text), write(" "), write(A), write(" "), writeln(B). 

loop_dfs(_, [], _).
loop_dfs(Node, [Next_node | Tails], Graph) :-
    /*Backedge, stops dfs when found*/
    (visited(Next_node), \+ parent(Node, Next_node) ->
	 assertz(backedge(Next_node, Node))
    ;parent(Node, Next_node) ->
         loop_dfs(Node, Tails, Graph)
    ;\+ visited(Next_node) ->
         assertz(parent(Next_node, Node)),
         dfs_from_node(Next_node, Graph),
         loop_dfs(Node, Tails, Graph)
    ).

dfs_from_node(Node, Graph) :-
    assertz(visited(Node)),
    neighbours(Node, Graph, Neighbours),
    loop_dfs(Node, Neighbours, Graph).

/*----------------------------------
Checks if graph is connected
----------------------------------*/
connected(Graph) :-
    reachable(1, Graph, Reachable_nodes),
    vertices(Graph, V),
    V = Reachable_nodes.

/*----------------------------------
Requires predicate parent, as well as 
two endpoints of a cycle, in order
to find all its nodes.
----------------------------------*/
find_cycle(End, End, Cycle) :-
    Cycle = [End].
find_cycle(End, Curr, Cycle) :-
    parent(Curr, Next),
    find_cycle(End, Next, Cycle_rest),
    Cycle = [Curr | Cycle_rest].

/*---------------------------------
Loops through list of the cycle
and removes all edges of the path
---------------------------------*/
loop_separate(Graph, [_], Forest) :-
    Forest = Graph.
loop_separate(Graph, [Curr, Next | Rest], Forest) :-
    delete_undirected_edge(Graph, Curr, Next, G),
    loop_separate(G, [Next | Rest], Forest).

separate_trees(Graph, Cycle, Forest) :-
    [First | _] = Cycle,
    last(Cycle, Last),
    delete_undirected_edge(Graph, First, Last, G),
    loop_separate(G, Cycle, Forest). 

/*----------------------------------------
Returns the sizes of the trees in a forest
----------------------------------------*/
count_tree_sizes(_, [], Tree_sizes) :-
    Tree_sizes = [].
count_tree_sizes(Forest, [Node | Rest_nodes], Tree_sizes) :-
    dfs_count(Node, Forest, Size),
    count_tree_sizes(Forest, Rest_nodes, Rest),
    Tree_sizes = [Size | Rest].

/*------------------------------------
Solves the problem on one graph
------------------------------------*/
do_coronograph(Graph, N, M, Ans) :-
    (N =:= M, connected(Graph), dfs_from_node(1, Graph) ->
	 backedge(First, Last),
	 find_cycle(First, Last, Cycle),
	 length(Cycle, L),
	 separate_trees(Graph, Cycle, Forest),
	 count_tree_sizes(Forest, Cycle, Tree_sizes),
	 msort(Tree_sizes, Sorted_sizes),
	 Ans = [L, Sorted_sizes]
    ;Ans = ['NO CORONA']
    ).

/*-----------------------------------
Reads the contents of a single line
-----------------------------------*/
read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

/*-------------------------------------
Loops through M edges, reads them and 
adds them to graph
-------------------------------------*/
read_add_edges(_, G, 0, Graph) :-
    Graph = G.
read_add_edges(Stream, G, M, Graph) :-
    M > 0,
    read_line(Stream, [U,V]),
    add_undirected_edge(G, U, V, Gnew),
    Mnew is M - 1,
    read_add_edges(Stream, Gnew, Mnew, Graph).

/*---------------------------------------
Loops through the T different testcases,
initialises visited and parent predicates
---------------------------------------*/
loop_testcase(_, 0, Answer) :-
    Answer = [].
loop_testcase(Stream, T, Answer) :-
    T > 0,
    read_line(Stream, [N, M]),
    create_graph(N, G),
    read_add_edges(Stream, G, M, Graph),
    retractall(visited(_)), retractall(parent(_,_)), retractall(backedge(_,_)),
    !,
    do_coronograph(Graph, N, M, Solution),
    Tnew is T - 1,
    !,
    loop_testcase(Stream, Tnew, Rest_solutions),
    Answer = [Solution | Rest_solutions].
    
coronograph(File, Answer) :-
    open(File, read, Stream),
    read_line(Stream, T),
    loop_testcase(Stream, T, Answer).

test_final() :-
    coronograph('corona.txt', Ans), writeln(Ans), fail. 
