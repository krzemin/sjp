% Piotr Krzemiński, Semantyka Języków Programowania, 2013/14
% Lista 1, zad. 1


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
aexp(subl(A1, A2), S, N) :-
	aexp(A1, S, N1),
	aexp(A2, S, N2),
	N is N1 - N2.
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
bexp(eq(A1, A2), S) :-
	aexp(A1, S, N),
	aexp(A2, S, N).

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
cexp(if(_, _, C), S, S1) :-
	cexp(C, S, S1).
cexp(while(B, C), S, S2) :-
	bexp(B, S), !,
	cexp(C, S, S1),
	cexp(while(B, C), S1, S2).
cexp(while(_, _), S, S).


execute(Program) :-
	nl,
	write('Program: '), write(Program), nl,
	write('Initial memory: '), write([]), nl,
	cexp(Program, [], S),
	write('Final memory: '), write(S), nl.


main(_) :-

	%% x := 10;
	%% x := 3 + x

	P1 = seq(assign(x, num(10)),
			 assign(x, add(num(3),var(x)))),
	execute(P1),

	%% x := 5
	%% if (x == 20) or (x <= 5) {
	%%   x := 10 * x
	%% } else {
	%%   skip
	%% }
	
	P2 = seq(assign(x, num(5)),
			 if(or(eq(var(x), num(20)), leq(var(x), num(5))),
			 	assign(x,mul(num(10),var(x))),
			 	skip)),
	execute(P2),

	%% x := 1;
	%% y := 1;
	%% z := 5;
	%% while x <= z {
	%%   y := y * x;
	%%   x := x + 1
	%% }

	P3 = seq(assign(x, num(1)),
		 seq(assign(y, num(1)),
		 seq(assign(z, num(5)),
		 	while(leq(var(x), var(z)),
		 		seq(assign(y, mul(var(y), var(x))),
		 			assign(x, add(var(x), num(1)))))
		 	))),
	execute(P3),

	%% n := 12;
	%% f1 := 1;
	%% f2 := 1;
	%% if n == 0 {
	%% 	 result := f1
	%% } else {
	%% 	 if n == 1 {
	%% 	   result := f2
	%% 	 } else {
	%% 	   f3 := f1 + f2;
	%% 	   i := 2;
	%% 	   while not (i == n) {
	%% 	     tmp := f3 + f2;
	%% 	     f1 := f2;
	%% 	     f2 := f3;
	%% 	     f3 := tmp;
	%% 	     i := i + 1
	%% 	   };
	%% 	   result := f3
	%% 	 }
	%% }

	P4 = seq(assign(n, num(12)),
		 seq(assign(f1, num(1)),
		 seq(assign(f2, num(1)),
		 if(eq(var(n), num(0)),
		 	assign(result, var(f1)),
		 	if(eq(var(n), num(1)),
		 		assign(result, var(f2)),
		 		seq(assign(f3, add(var(f1), var(f2))),
		 		seq(assign(i, num(2)),
		 		seq(while(not(eq(var(i),var(n))),
		 			seq(assign(tmp, add(var(f3), var(f2))),
		 			seq(assign(f1, var(f2)),
		 			seq(assign(f2, var(f3)),
		 			seq(assign(f3, var(tmp)),
		 				assign(i, add(var(i), num(1)))))))
		 			),
		 			assign(result, var(f3)))))
		 		)
		 	)
		 	))),
	execute(P4).


