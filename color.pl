%%%% Coloring a graph

%%% A graph is a list of nodes.
%%% A node is made with the functor node with arguments:
%%%  - name, a symbol (e.g. a or x)
%%%  - color, an integer
%%%  - neighbors, a list of node names (Note:  just the names, not the node(...)
%%%    structures)
%%% such that no two nodes in a graph have the same name and if a is a
%%% neighbor of b, b is a neighbor of a.

%%% Note:  In the following Prolog predicates (i.e. rules), a + sign
%%% in front of an argument means input, and a - sign in front of an
%%% argument means an output.

%%% node_structure(+Name, +Graph, -Structure)  Graph is assumed to be a
%%% graph and Name is assumed to be the name of one of its nodes.  Structure
%%% is the node structure in Graph with name Name.
node_structure(Name, [node(Name, Color, Nbrs) | _],node(Name, Color, Nbrs)).
node_structure(Name, [_ | T], Structure):- node_structure(Name,T, Structure).

%%% all_names(+Graph, -Names) Graph is a list of nodes structures, Names is
%%% a list of just the names from the nodes in Graph

%%% fill in all_names here
all_names(node(Name, _, _), Name).
all_names(Graph, Names) :- maplist(all_names, Graph, Names).


%%% all_unique(+List) List is assumed to be a list of symbols.
%%% all_unique is true if and only if there are no duplicate symbols in List.
%%% E.g., all_unique([a, b, c]) is true but all_unique([a, b, a]) is false.

%%% fill in all_unique here
all_unique([]).
all_unique([H|T]):- (\+member(H,T)) , all_unique(T).

%%% node_backlinks_valid(+Name, +Nbrs, +Graph) Name is assumed to be a node name,
%%% Nbrs is assumed to be a list of some of its neighbor names, and
%%% Graph is assumed to be a graph.   True if and only if every
%%% node in Nbrs has Name as a neighbor in Graph.  E.g. if
%%% G is [node(c, 0, [ ]), node(b, 0, [a, c]), node(a, 0, [b])]
%%% node_backlinks_valid(a, [b], G) is true but 
%%% node_backlinks_valid(b, [a, c]), G) is false because b is not in c
%%% neighbor list in G

%%% fill in node_backlinks_valid here
node_backlinks_valid(_,[],_).
node_backlinks_valid( Name, [H|T], Graph):- node_structure(H, Graph, node(H,_,Structure)), member(Name,Structure), node_backlinks_valid(Name, T, Graph).

%%% all_backlinks_valid(+Nodes, +Graph) Nodes is assumed to be a list of node
%%% structures, Graph a graph.  It is true if and only if all the nodes in Nodes
%%% are backlink valid in Graph.

%%% fill in all_backlinks_valid here
all_backlinks_valid([], _).
all_backlinks_valid([node(Name,_,Structure)|T] , Graph):- node_backlinks_valid(Name ,Structure , Graph), all_backlinks_valid( T ,Graph).

%%% graph_valid(+G) G is assumed to be a graph.  It is true iff all the node
%%% names in G are unique and all the backlinks in G are valid
graph_valid(G):-
	all_names(G, Names), all_unique(Names), all_backlinks_valid(G, G).

%max_color(k) means k is the number of colors allowed
max_color(3).

%%% color(-N) N is an integer in the range from 1 thru max color

%%% fill in color here
color_help(X,X,[X]).
color_help(H,X,[H|T]):- H < X, N is H + 1, color_help(N,X,T).
color(N):- max_color(K), color_help(1, K, V), member(N, V).

%%% colors_of(+Names, +Graph, -Colors) Names is a list of node names, Graph is a
%%% list of node structures, Colors is the list of colors that Nodes have in
%%% Graph

%%% fill in colors_of here
colors_of([], _, []).
colors_of([H|T],Graph,[Y|Z]):-node_structure(H,Graph, node(H,Y,_)), colors_of(T, Graph, Z).

%%% valid_colors(+Nodes, +Graph) Nodes is a list of node structures,
%%% Graph is a graph.  Nodes is a tail (cdr of cdr ... of cdr) of Graph.
%%% True iff each nodes in Nodes has a
%%% different color than its neighbors in Graph.
valid_colors([ ], _).
valid_colors([node(_,Color,Nbrs) | Nodes], All_nodes):-
	colors_of(Nbrs, All_nodes, Colors),
	\+(member(Color, Colors)),
	valid_colors(Nodes, All_nodes).

%%% valid_colors(+G) G is assumed to be a graph, True iff G is legally
%%% colored, ie if no arc links two nodes of the same color
valid_colors(All_nodes):-valid_colors(All_nodes, All_nodes).

%%% colored_nodes(Nodes) is a list of node structures with some color
%%% filled in for each structure
colored_nodes([ ]).
colored_nodes([node(_, Color, _) | Nodes]):-
	color(Color),
	colored_nodes(Nodes).

%%% solve_coloring(+Nodes) Nodes is a graph, ie a coloring problem,
%%% Chooses a valid color for each node in Nodes

%%% fill in solve_coloring here
solve_coloring(N):-graph_valid(N),colored_nodes(N),valid_colors(N).
