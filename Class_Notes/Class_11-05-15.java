/* 
	Last Time:
	----------
	Test Driven Development:
	define interface, 
	test interface,
	use tests to drive development.

	Today:
	------
	Java supports two kinds of polymorphism
		1. parametric polymorphism
			-Generics
			-Seen in ML
			Ex:
				List<String> stringList;
				List<Shape> shapeList;
			Can also use this to make interfaces more generic:

				interface Set<T> {
					void add(T e);
					boolean contains(T e);
				}
			** Can make just about anything 'generic' or polymetric **
				-Can also make non-parametric methods
		2. Subtype polymorphism
			Ex:
				interface RemovableSet extends StringSet {
					void remove(String s);
				}
			-Can extend multiple interfaces simultanously
			-Creates a 'is a' relationship = subtype relationship
			

*/