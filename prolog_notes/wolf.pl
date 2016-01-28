%% Wolf, Goat, Cabbage Problem...

%% Moves: wolf, goat, cabbage, or none

%% move(Config1, Move, Config2)
	%% Move makes sense...
	%% Needs to be a safe move...

%% Use w (west) and e (east) for sides of river
%% [Person, Wolf, Goat, Cabbage]

%% Initial = [w,w,w,w]
%% Goal    = [e,e,e,e]

opposite(w,e).
opposite(e,w).

%% Moving alone:
	%% opposite(W, G).
	%% opposite(G, C).
move([P, W, G, C], none, [Pnew, W, G, C]) :-
	opposite(W, G),
	opposite(G, C),
	opposite(P, Pnew).

move([Pw, Pw, G, C], wolf, [PWnew, PWnew, G, C]) :-
	opposite(G, C),
	opposite(Pw, PWnew).

move([Pg, W, Pg, C], goat, [PGnew, W, PGnew, C]) :-
	opposite(Pg, PGnew).

move([Pc, W, G, Pc], cabbage, [PCnew, W, G, PCnew]) :-
	opposite(Pc, PCnew),
	opposite(W, G).

puzzle(Start, [], Start).
puzzle(Start, [Hd|Tl], End) :- move(Start, Hd, Inter), puzzle(Inter, Tl, End).


