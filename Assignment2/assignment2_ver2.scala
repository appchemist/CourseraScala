object assignment2 {
	type Set = Int => Boolean
	
	val MAX = 1000
	val MIN = -1000
	
	def contains(s: Set, elem: Int): Boolean = s(elem)

	def singletonSet(elem: Int): Set = (x : Int) => x == elem
	
	def union(s: Set, t: Set): Set = (x : Int) => s(x) || t(x)

	def intersect(s: Set, t: Set): Set = (x : Int) => s(x) && t(x)

	def diff(s: Set, t: Set): Set = (x : Int) => s(x) && !t(x)

	def filter(s: Set, p: Int => Boolean): Set = (x : Int) => s(x) && p(x)
	
	def forall(s: Set, p: Int => Boolean): Boolean = {
		def iter(a: Int): Boolean = {
			if (a > MAX) return true;
			else if (s(a) && !p(a)) return false;
			else iter(a+1)
		}
		iter(MIN)
	}
	
	def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
	
	def map(s: Set, f: Int => Int): Set = {
		def iter(a: Int, acc: Set): Set = {
			if (a > MAX) return acc
			else if (s(a)) iter(a+1, union(acc, x => f(a) == x))
			else iter(a+1, acc)
		}
		iter(MIN, x => false)
	}
		 
	def toString(s: Set): String = {
    	val xs = for (i <- MIN to MAX if contains(s, i)) yield i
    	xs.mkString("{", ",", "}")
	}

	/**
	 * Prints the contents of a set on the console.
	 */
	def printSet(s: Set) {
		println(toString(s))
	}
}
