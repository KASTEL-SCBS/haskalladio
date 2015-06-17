:- dynamic planning/1, on/3, between1/3, moved/3, ic/0, other_move/3,
   initially/3.

planning([]).
planning([on(X,Y,T)|Rest]):-
	on(X,Y,T),
        write('Move '),write(X),write(' to '),write(Y),writeln(' done.'),
	planning(Rest).


on(X,Y,T) :- initially(X,Y,T0), T0 #<= T, not moved(X,T0,T).
on(X,Y,T) :- T1 #>0, T1 #<= T, not moved(X,T1,T),move(X,Y,T1). 


ic:- not_moved(X,T1,T2), move(X,Z,T), T1 \== T,between1(T,T1,T2).

ic:- move(X1,Y1,T1), other_move(X2,Y2,T2), X1=\=X2, T1 #= T2.
ic:- move(X,Y1,T1), other_move(X,Y2,T2), Y1\==Y2, T1 #= T2.

ic:- move(X,Y,T), initially(Z,X,T1), not moved(Z,T1,T).
ic:- move(X,Y,T), move(Z, X1, T1), T \== T1, not moved(Z,T1,T),
     T#>=T1 #/\ X #= X1.

ic:- move(X,Y,T), number(Y), initially(Z,Y,T1),not moved(Z,T1,T).
ic:- move(X,Y,T), move(Z,Y1,T1), T \== T1, not moved(Z,T1,T),
     T #>= T1 #/\ Y #= Y1.

% Conditions T \== T1 are used to prevent unnecessary checking
% of consistency of a move with itself.

ic:- move(X,Y,T), initially(Z,Y1,T1), nott moved(Z,T1,T), Y #= Y1.

% Specify in the next rule the total number of possible positions.

moved(X,T1,T2) :- Y::1..22, Y##X, T1#<T2, between1(T9,T1,T2), move(X,Y,T9).

between1(T3, T1,T2) :-
	T3 #> T1, T3 #< T2.

other_move(X,Y,T) :- move(X,Y,T).

abducible_predicate(move/3).
abducible_predicate(not_moved/3).

% Consult also the Initial State
