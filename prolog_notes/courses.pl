prereq(cmsi281,cmsi185).  % Data Structures depends on Computer Programming
prereq(cmsi282,cmsi281).  % Algorithms depends on Data Structures
prereq(cmsi284,cmsi281).  % Systems Programming depends on Data Structures
prereq(cmsi355,cmsi284).  % Networks and Internets depends on Systems Programming
prereq(cmsi386,cmsi284).  % Programming Languages depends on Systems Programming
prereq(cmsi387,cmsi284).  % Operating Systems depends on Systems Programming
prereq(cmsi485,cmsi385).  % Artificial Intelligence depends on Theory of Computation
prereq(cmsi485,cmsi386).  % Artificial Intelligence depends on Programming Languages
prereq(cmsi486,cmsi386).  % Intro to Database Systems depends on Programming Languages
prereq(cmsi486,cmsi387).  % Intro to Database Systems depends on Operating Systems

prereq2(X,Y) :- prereq(X,Z), prereq(Z,Y).


prereqEI(X,Y) :- prereq(X,Y).
%prereqEI(X,Y) :- prereq2(X,Y).
%prereqEI(X,Y) :- prereq2(X,Z), prereq2(Z,Y). % 3 intermediates
%prereqEI(X,Y) :- prereq(X,Z), prereq2(Z,Y). % 2 intermediates
prereqEI(X,Y) :- prereq(X,Z), prereqEI(Z,Y). % N intermediates


eval(1+2,3).

myAppend([],L,L).
myAppend([Hd|Tl], L, [Hd|R]) :- myAppend(Tl, L, R).