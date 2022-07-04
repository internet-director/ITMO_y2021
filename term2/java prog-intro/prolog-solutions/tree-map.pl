new_node(K1, V1, [(K1, V1), nil, nil, P]) :- rand_float(P).

merge(L, nil, L).
merge(nil, R, R) :- R \= nil.
merge([(K1, V1), L1, R1, P1], [(K2, V2), L2, R2, P2], O) :-
	(P1 >= P2) ->
	merge(R1, [(K2, V2), L2, R2, P2], O1),
	O = [(K1, V1), L1, O1, P1];
	merge([(K1, V1), L1, R1, P1], L2, O2),
	O = [(K2, V2), O2, R2, P1].

split(nil, K, nil, nil).
split([(K1, V1), L1, R1, P1], K2, Ol, Or) :-
	(K2 > K1) ->
	split(R1, K2, D1, Or),
	Ol = [(K1, V1), L1, D1, P1];
	split(L1, K2, Ol, D2),
	Or = [(K1, V1), D2, R1, P1].

map_get([(K, V), L, R, P], K, V).
map_get([(K, V), L, R, P], K1, V1) :-
	(K > K1) ->
	map_get(L, K1, V1);
	map_get(R, K1, V1).

map_find([(K, V), L, R, P], K).
map_find([(K, V), L, R, P], K1) :-
	(K > K1) ->
	map_find(L, K1);
	map_find(R, K1).

map_remove(T, K, O) :-
	split(T, K, L, R),
	split(R, K + 1, D, R1),
	merge(L, R1, O).

add((K, V), nil, O) :- new_node(K, V, O).
add((K, V), [(K1, V1), L1, R1, P1], O) :-
	split([(K1, V1), L1, R1, P1], K, L, R2),
	new_node(K, V, N),
	merge(L, N, L2),
	merge(L2, R2, O).

map_put(T, K, V, O) :- 
	map_remove(T, K, D),
	add((K, V), D, O).

map_replace(T, K, V, T) :-
	\+ map_find(T, K), !.
map_replace(T, K, V, R) :-
	map_put(T, K, V, R).

map_find_floor(nil, K1, K2, O) :- O = K1.
map_find_floor([(K, V), L, R, P], K1, K2, O) :- K2 < K, O = K1, !.
map_find_floor([(K, V), L, R, P], K1, K2, O) :- K2 =:= K, O = K, !.
map_find_floor([(K, V), L, R, P], K1, K2, O) :- 
	(K >= K2) ->
	map_find_floor(L, K, K2, O);
	map_find_floor(R, K, K2, O).

map_floorKey([(K, V), L, R, P], K1, O) :- 
	map_find_floor(T, K - 1, K1, O).
		
init([], T, T).
init([(K, V) | T], Tr, M) :-
	map_put(Tr, K, V, O),
	init(T, O, M).
map_build(L, T) :- 
	init(L, nil, T).