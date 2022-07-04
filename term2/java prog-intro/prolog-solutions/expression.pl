:- load_library('alice.tuprolog.lib.DCGLibrary').

nonvar(V, _) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

digits_p([]) --> [].
digits_p([H | T]) --> { member(H, [' ']) }, !.
digits_p([H | T]) --> 
	{ member(H, ['-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])},
  [H], digits_p(T).
  
var_p([]) --> [].
var_p([H | T]) --> { member(H, [' ']) }, !.
var_p([H | T]) --> 
  { member(H, ['x', 'y', 'z', 'X', 'Y', 'Z'])},
  [H], var_p(T).

skip([], []) :- !.
skip([' ' | H], R) :- skip(H, R), !.
skip([T | H], [T | R]) :- skip(H, R).

fix([], []).
fix(['(', '-' | T], ['(', '-' | R]) :- fix(T, R), !.
fix([H, K | T], [H, ' ', K, ' ' | R1]) :- op_r(K), fix(T, R1),  !.
fix(['n', 'e', 'g', 'a', 't', 'e' | T], ['n', 'e', 'g', 'a', 't', 'e' | R]) :- fix(T, R), !.
fix([H | T], [H | R]) :- fix(T, R).

skip_f([' ' | H], H).
skip_f([T | H], [T | H]).
%skip_l([T | ' '], T). 

first([H | T], H).
isMin(['-' | T]).

expr_p(const(X)) -->
  { nonvar(X, number_chars(X, C)) },
  digits_p(C),
  { C = [_, _ | _], number_chars(X, C) }.
expr_p(variable(X)) -->
  { nonvar(X, atom_chars(X, C)) },
  var_p(C),
  { C = [_ | _], atom_chars(X, C) }.
expr_p(operation(Op, A)) --> oper_s(Op), ['('], expr_p(A), [')'].
expr_p(operation(Op, A, B)) --> ['('], expr_p(A), [' '], oper_s(Op), [' '], expr_p(B), [')'].

oper(op_subtract, '-').
oper(op_add, '+').
oper(op_multiply, '*').
oper(op_divide, '/').
oper(op_negate, ['n', 'e', 'g', 'a', 't', 'e']).
oper_dop(op_negate, 'negate').

oper_s(op_subtract) --> ['-'].
oper_s(op_add) --> ['+'].
oper_s(op_multiply) --> ['*'].
oper_s(op_divide) --> ['/'].
oper_s(op_negate) --> ['n', 'e', 'g', 'a', 't', 'e'].

op_r('+').
op_r('-').
op_r('*').
op_r('/').
%op_r('n', 'e', 'g', 'a', 't', 'e').
op_r(['n', 'e', 'g', 'a', 't', 'e']).

calc(['+', L, R], O) :- O is L + R.
calc(['*', L, R], O) :- O is L * R.
calc(['/', L, R], O) :- O is L / R.
calc(['-', L, R], O) :- O is L - R.
calc(['n', 'e', 'g', 'a', 't', 'e'], L, R) :- R is -L.
%calc('negate', L, -L).

evaluate(const(X), Var, X).
evaluate(variable(X), Var, Res) :- atom_chars(X, C), first(C, C1), lookup(C1, Var, Res).
evaluate(operation(O, L), Var, Res) :- 
	evaluate(L, Var, Res1),
	oper(O, T),
	calc(T, Res1, Res).
evaluate(operation(O, L, R), Var, Res) :- 
	evaluate(L, Var, Res1),
	evaluate(R, Var, Res2),
	oper(O, T),
	calc([T, Res1, Res2], Res).


infix_str(E, A) :- ground(E), phrase(expr_p(E), C), atom_chars(A, C).
infix_str(E, A) :-   atom(A), atom_chars(A, C), skip(C, F), fix(F, F1), phrase(expr_p(E), F1). 	