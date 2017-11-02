package funsets

// https://stackoverflow.com/questions/39063778/scala-characteristic-function
/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean
  //set is a purely functional
  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
    def singletonSet(elem: Int): Set = {
      (a: Int) => a == elem
    }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = {
      (elem:Int) => s(elem) || t(elem)
    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {
      (elem:Int) => s(elem)&& t(elem)
    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {
      (elem:Int) => s(elem) && !t(elem)
    }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {
      //(elem: Int) => if(p(elem)) s(elem) else false
      (elem: Int) => s(elem) && p(elem)
    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a > bound) true
        else if (contains(diff(s,p),a)) false
        else iter(bound+1)
      }
      iter(-bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  //not all elements in s satisfies !p, which means at least one element in s satisfies p
  //this doesn't care about a bound, it simply is a proof?
    def exists(s: Set, p: Int => Boolean): Boolean = {!forall(s,x => !p(x))}

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  //let's say set2 = map(set1,f)
  //the only way set2(x) is true is if there exists a y in set1 such that f(y) == x
    def map(s: Set, f: Int => Int): Set = {
      (a:Int) => exists(s,y => y == f(a))
    }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
