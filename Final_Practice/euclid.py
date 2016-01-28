# The Algorithm:
#      If the two numbers are equal, the gcd is that number.
#      If either number is zero, the gcd is the non-zero number.
#      Otherwise, one number is greater than the other.
#        the gcd of the two numbers is equal to the gcd of the smaller
#        number and the difference between the two numbers.

#  b) Implement gcd in an imperative style (loops and update instead
#       of recursion) in python or java.

def gcd(x,y):
	while y:
		x, y = y, x%y
	return x