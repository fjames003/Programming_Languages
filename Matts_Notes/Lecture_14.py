"""
Python
  - is a snake
  - its a british comedy thing
  - its always there
  - its dynamically typed
  - its strongly typed
  - no stupid semicolons
  - multi-paradigm paradigm
  - no stupid curly braces
  - lots of stupid indentation

Its a scripting language.
  - others: Perl, Ruby, Javascript, (BASH) Shell script

What does it mean?
  - interpreted 
  - interact with (automation) or extend (plugin) some larger program or system
    - web browser (javascript), games (lua), excel (VB), OS command line (BASH)
    - PHP (web server)
  - "scripting" now basically synonymous with "dynamically typed" 
  - tend to see:
    - easy text manipulation
    - easy file and file system stuff
    - invocation of other programs
  - rapid prototyping
    - lots of libraries 
    - reasonable performance

Scripting languages are often multi-paradigm, and often *messy*
  - mixing different features from different paradigms
  - lots of ways to do the same thing
  - error-prone: can combine in unexpected ways

  - plus: usually some way to do what you need relatively easily

Python:
  - imperative/functional/OO
  - we'll focus on first two
  - functional programming in the real world.
    side effects: I/O, mutation, etc
  - this style increasingly important.
     Javascript, Java, C++

"""

# this is a comment. What follows is code.
# You should paste each thing into python's repl.
# We're using python 3
# Get it: https://www.python.org/
# Learn about it:
#   https://www.python.org/about/gettingstarted/
#   http://www.diveintopython3.net/
# If you're used to python2, check out:
#   https://docs.python.org/3/whatsnew/


# this is a tuple.
(1, "hi")

# tuples in python are immutable lists.
tup = (1, 'hi')
tup[0]
tup[0] = 2 # error

# unlike ocaml, lists in python are
# heterogeneous (thanks dynamic typing!)
l = [1,2,"hi",4]

# and mutable. 
l[0] = 2 # no problem.

# slicing is a thing.
# what do these do? (try it)
l[1:3]
l[1:]
l[:3]

# negative indices
l[-1]
l[:-1]
l[-1:]

# black slice magic
l[::-1] # hmmm???

# explicit conversion between types
int('34')
list('34')
tuple('34')

# this is a function
def prodList(l):
    result = 1
    for i in l:
        result *= i
    return result

# try each of these.
prodList([1,2,3,4,5])
prodList([1,2,"hello",4,5])
prodList([1,2,"hello","hello",5])

# we can use run-time type tests to
# get a grip on heterogeneity 

def prodList(l):
  result = 1
  for i in l:
    if type(i) == int:
      result *= i
    elif type(i) == str:
      result *= int(i)
    else:
      raise Exception("hmmmm?", i)
  return result 

# for loops work on many things

# tuples
for i in (1,2,3,4):
  print(i)

# lists
for i in [1,2,3,4]:
  print(i)

# strings
for c in 'hello':
  print(c)
  
# on-line help system.

help(5)  # read help for ints
help([1,2,3]) # read help for lists
help((1,2,3)) # or for tuples
help('for') # or for loops
help(str) # or strings


# + works for many types.

1 + 2
[1,2,3] + [4,5,6]
[1,2,3] + 4 # no thank you.
[1,2,3] + [4] # yes please.

