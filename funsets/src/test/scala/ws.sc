object FunSets
{
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem


  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s,x) || contains(t,x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s,x) && contains(t,x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s,x) && !contains(t,x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s,x) && p(x)



  val a = contains(x => true, 100)

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s5 = singletonSet(5)
  val s1000 = singletonSet(-1000)
  var u = union(s1,s2)
  u = union(u,s3)
  u = union(u,s5)
  u = union(u,s1000)

  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (contains(s,a) && !p(a)) false
      else iter(a+1)
    }
    iter(-1000)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x:Int)=> !p(x) )

  def map(s: Set, f: Int => Int): Set = (x:Int)=> exists(s, (y:Int) => f(y) ==x  )

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  val test = forall(u, (x:Int) => x>0)

  val test2= exists(u, (x:Int) => x==0)

  val str1 = toString(map(u, (x:Int) => x+1))






}