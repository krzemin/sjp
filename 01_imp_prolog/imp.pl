
% interpreter

lookupState([], _, _) :- fail.
lookupState([(X,V)|_], X, V) :- !.
lookupState([_|T], X, V) :- lookupState(T, X, V).

insertState([], (X, V), [(X, V)]) :- !.
insertState([(X, _)|T], (X, V), [(X, V)|T]) :- !.
insertState([H|T], P, [H|T1]) :- insertState(T, P, T1).

aexp(num(N), _, N).
aexp(var(X), S, N) :- lookupState(S, X, N).
aexp(add(A1, A2), S, N) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N is N1 + N2.
aexp(mul(A1, A2), S, N) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N is N1 * N2.

bexp(t, _) :- !.
bexp(f, _) :- fail, !.
bexp(not(B), S) :- not(bexp(B, S)).
bexp(and(B1, B2), S) :-
	bexp(B1, S),
	bexp(B2, S).
bexp(or(B1, B2), S) :-
	bexp(B1, S), ! ;
	bexp(B2, S).
bexp(leq(A1, A2), S) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N1 =< N2.

cexp(skip, S, S) :- !.
cexp(assign(X, A), S, S1) :-
	aexp(A, S, N),
	insertState(S, (X, N), S1).
cexp(seq(C1, C2), S, S3) :-
	cexp(C1, S, S2),
	cexp(C2, S2, S3).
cexp(if(B, C, _), S, S1) :-
	bexp(B, S), !,
	cexp(C, S, S1).
cexp(if(B, _, C), S, S1) :-
	not(bexp(B, S)),
	cexp(C, S, S1).
cexp(while(B, _), S, S) :-
	not(bexp(B, S)), !.
cexp(while(B, C), S, S2) :-
	bexp(B, C),
	cexp(C, S, S1),
	cexp(while(B, C), S1, S2).


