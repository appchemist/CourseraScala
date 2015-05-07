object assignment2 {
	type Set = Int => Boolean
	
	val MAX = 1000
	val MIN = -1000
	
	def contains(s: Set, elem: Int): Boolean = s(elem)

	def singletonSet(elem: Int): Set = {
		def Set(a: Int): Boolean = {
			if (elem == a)
				return true;
			return false;
		}
		
		return Set
	}
	
	def union(s: Set, t: Set): Set = {
		def Set(a: Int): Boolean = {
			if (s(a) || t(a))
				return true;
			return false;
		}
		
		return Set;
	}

	def intersect(s: Set, t: Set): Set = {
		def Set(a: Int): Boolean = {
			if (s(a) && t(a))
				return true;
			return false;
		}
		
		return Set;
	}

	def diff(s: Set, t: Set): Set = {
		def Set(a: Int): Boolean = {
			if (s(a) && !t(a))
				return true;
			return false;
		}
		
		return Set;
	}
	
	def filter(s: Set, p: Int => Boolean): Set = {
		def Set(a: Int): Boolean = {
			if (s(a) && p(a))
				return true;
			return false;
		}
		
		return Set;
	}
	
	def forall(s: Set, p: Int => Boolean): Boolean = {
		@tailrec def iter(a: Int): Boolean = {
			if (a > MAX) return true;
			else if (s(a) && !p(a)) return false;
			else iter(a+1)
		}
		iter(MIN)
	}
	
	def exists(s: Set, p: Int => Boolean): Boolean = {
		var isExist = false
		
		def forexists(p: Int => Boolean)(a: Int): Boolean = {
			if (p(a)) {
				isExist = true
				return true
			}
			else {
				return false
			}
		}
		
		forall(s, forexists(p))
		return isExist
	}
	
	def map(s: Set, f: Int => Int): Set = {
		val MIN = -1000
		val MAX = 1000
		
		@tailrec def iter(a: Int, b: Set): Set = {
			if (a > MAX) return b
			else if (s(a)) {
				if (b == null) iter(a+1, singletonSet(f(a)))
				else iter(a+1, union(b, singletonSet(f(a))))
			}
			else iter(a+1, b)
		}
		iter(MIN, null)
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