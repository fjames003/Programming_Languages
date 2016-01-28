% Class_Notes_12-8-15

myAppend([],L,L).
myAppend([Hd|Tl], L, [Hd|R]) :- myAppend(Tl, L, R).

myAppend2(L1, L2, L3) :-
	L1 = [],
	L3 = L2.

myAppend2(L1, L2, L3) :-
	L1 = [Hd|Tl],
	myAppend2(Tl, L2, R), % R = append(Tl, L2)
	L3 = [Hd|R].          % return [Hd|R]

%% reverse(X,Y)
rev([],[]).
rev([Hd|Tl], R) :- reverse(Tl, TlR), append(TlR, [Hd], R).

%% cons(1, cons(2, nil))

app_(nil, L2, L2).
app_(cons(Hd, Tl), L2, cons(Hd, R)) :- app_(Tl, L2, R).


