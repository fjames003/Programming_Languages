%% c) Define a relation gcd(X,Y,D) in prolog that is true if D is the
%%       gcd of X and Y.

%% The Algorithm:
%%      If the two numbers are equal, the gcd is that number.
%%      If either number is zero, the gcd is the non-zero number.
%%      Otherwise, one number is greater than the other.
%%        the gcd of the two numbers is equal to the gcd of the smaller
%%        number and the difference between the two numbers.

gcd(X,X,X).
gcd(X,0,X).
gcd(0,Y,Y).
gcd(X,Y,Z) :- X > Y, A is X - Y, gcd(Y, A, Z).
gcd(X,Y,Z) :- Y > X, A is Y - X, gcd(X, A, Z).