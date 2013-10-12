
lookup([], _, _) :- fail.
lookup([(X,V)|_], X, V) :- !.
lookup([_|T], X, V) :- lookup(T, X, V).

aexp(num(N), _, N).
aexp(var(X), S, N) :- lookup(S, X, N).
aexp(add(A1, A2), S, N) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N is N1 + N2.
aexp(mul(A1, A2), S, N) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N is N1 * N2.




