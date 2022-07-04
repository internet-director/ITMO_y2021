init(N) :- init_rec(N, 2).
init_rec(N, D) :- N =< D * D, !.
init_rec(N, D) :- 
  D1 is D + 1,
  init_table(N, D, D),
  init_rec(N, D1).

init_table(N, D, O) :- D1 is D + O, N =< D1, !.
init_table(N, D, O) :-
  D1 is D + O,
  assert(prime_table(D1)),
  init_table(N, D1, O).

prime(N) :- \+ prime_table(N).
composite(N) :- prime_table(N).

prime_div(1, D) :- D is [], !.
prime_div(N, [H | T]) :- 
  0 is mod(N, H),
  N1 is N // H, 
  prime_div(N1, T), !.

prime_dop(N, C, [H | T]) :- N < 2, !.
prime_dop(N, C, [H]) :- prime(N), H is N, !.
prime_dop(N, C, [H | T]) :-
  N >= C, 
  prime(C),
  0 is mod(N, C),
  H is C,
  N1 is N // C,
  prime_dop(N1, C, T), !.
prime_dop(N, C, [H | T]) :- 
  N >= C,
  C1 is C + 1,
  prime_dop(N, C1, [H | T]), !.

prime_mul(N, []) :- N is 1, true, !.
prime_mul(N, [H | T]) :-
  prime_mul(N1, T),
  N is H * N1.

erase([_], []) :- true, !.
erase([H | T], [H | T1]) :- erase(T, T1).

sort(0, [H | T]) :- sort(H, T), !.
sort(N, []) :- true, !.
sort(N, [H | T]) :-
  N =< H,
  prime(H),
  sort(H, T).

prime_divisors(1, []) :- true, !.
prime_divisors(N, [D | T]) :- number(D), \+ prime(D), !.
prime_divisors(N, T) :-
  list(T),
  number(N),
  prime_div(N, T), !.
prime_divisors(N, T) :- 
  number(N),
  prime_dop(N, 2, T), !.
prime_divisors(N, T) :- 
  sort(0, T),
  prime_mul(N, T).

nth_prime(N, P) :- 
  number(N),
  nth_dop(N, 1, P), !.
nth_prime(N, P) :- 
  nth_pr(N, 2, 1, P), !.

next(P, R) :- P1 is P + 1, prime(P1), R is P1, !.
next(P, R) :-
  P1 is P + 1,
  next(P1, R).
  
nth_dop(N, P, I) :- N is 0, I is P, !.
nth_dop(N, P, I) :-
  N1 is N - 1,
  next(P, K),
  nth_dop(N1, K, I).

nth_pr(N, S, C, P) :- S is P, N is C, !.
nth_pr(N, S, C, P) :-
  next(S, S1),
  C1 is C + 1,
  nth_pr(N, S1, C1, P).

lcm(A, B, LCM) :- 
  prime_divisors(A, A1),
  prime_divisors(B, B1),
  lcm_dop(A1, B1, L), 
  lcm_mul(L, LCM),  !.

lcm_dop(A, [], A) :- !.
lcm_dop([], B, B) :- !.
lcm_dop([H1 | T1], [H2 | T2], LCM) :- H1 =:= H2, lcm_dop(T1, T2, LCM1), LCM = [H1 | LCM1], !.
lcm_dop([H1 | T1], [H2 | T2], LCM) :- H1 < H2, lcm_dop(T1, [H2 | T2], LCM1), LCM = [H1 | LCM1], !.
lcm_dop([H1 | T1], [H2 | T2], LCM) :- H1 > H2, lcm_dop([H1 | T1], T2, LCM1), LCM = [H2 | LCM1], !.  

lcm_mul([], 1).
lcm_mul([H], H), !.
lcm_mul([H | T], R) :- lcm_mul(T, R1), R is H * R1, !.