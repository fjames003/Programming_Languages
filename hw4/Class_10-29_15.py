#tuple to list
import math
def primesTo(n):
	def noDivisors(n):
		for i in range(2, int(math.sqrt(n)) + 1):
			if n % i == 0:
				return False
		return True
	return list(filter(lambda y : y is not None, [x if noDivisors(x) else None for x in range(2,n+1)]))