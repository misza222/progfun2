println("Experiments for Week1")


// Example of defining map and flatMap for using certaing DT in for expressions
// a bit background on expansion rules

trait GeneratorWeak[+T] {
  def generate: T
}

val integers = new GeneratorWeak[Int] {
  val rand = new java.util.Random

  def generate:Int = rand.nextInt()
}

integers.generate

// // At this point we cannot create booleans generator with for
//val booleans = for { i : Int <- integers } yield i > 0
// or pairs generator like
//
//def pairs[T, U](u: GeneratorWeak[U], t: GeneratorWeak[T]): GeneratorWeak[(T, U)] = for {
//    x <- u
//    y <- t
//  } yield (x, y)
//
//pairs(integers, booleans).generate

// we can do normal boolean generator though
val booleans = new GeneratorWeak[Boolean] {
  override def generate: Boolean = integers.generate > 0
}
booleans.generate

// and normal pair generator
val pairs = new GeneratorWeak[(Integer, Boolean)] {
  override def generate: (Integer, Boolean) = (integers.generate, booleans.generate)
}
pairs.generate

// to be able to do this with for, we need to define map and flatMap
// according to expansion rules for
// [for](https://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html#for-comprehensions-and-for-loops)

trait Generator[+T] {
  upper =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(upper.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(upper.generate).generate
  }
}

val integers2 = new Generator[Int] {
  val rand = new java.util.Random()

  override def generate: Int = rand.nextInt()
}

// with map defined in Generator we can already do booleans generator with for
val booleans2: Generator[Boolean] = for {i <- integers2} yield i > 0

booleans2.generate
booleans2.generate
booleans2.generate

// and with flatMap we can generate generators with multiple enumerator expressions

def pairs2[T, S] (t: Generator[T], s: Generator[S]): Generator[(T, S)] = for{
  x <- t
  y <- s
} yield (x, y)

val pairs_generator = pairs2(integers2, booleans2)
pairs_generator.generate

//// END of post

def single[T](v: T): Generator[T] = new Generator[T] {
  override def generate: T = v
}

def choose(lo: Int, hi: Int): Generator[Int] = {
  for (x <- integers2) yield lo + x % (hi - lo)
}

def oneOf[T](xs: T*): Generator[T] = {
  println(xs)
  for (index <- choose(0, xs.length)) yield xs(index)
}

single("Ala").generate

val oof = oneOf("ala", "jola", "hela")
//oof.generate


val emptyList = single(Nil)
def nonEmptyLists = for {
  x <- integers2
  tail <- lists
} yield x :: tail

def lists: Generator[List[Int]] = for {
  isEmpty <- booleans2
  list <- if (isEmpty) emptyList else nonEmptyLists
} yield list

lists.generate
lists.generate
lists.generate
lists.generate
lists.generate


trait Tree

case class Inner(l: Tree, r: Tree) extends Tree
case class Leaf(x: Int) extends Tree


def trees: Generator[Tree] = for {
  isLeaf <- booleans2
  tree <- if(isLeaf) leaf else notLeaf
}  yield tree

def leaf: Generator[Leaf] = for {
  x <- integers2
} yield new Leaf(x)

def notLeaf: Generator[Tree] = for {
  ltree <- trees
  rtree <- trees
} yield new Inner(ltree, rtree)

trees.generate
trees.generate
trees.generate
trees.generate

//// 05
//
//trait GeneratorWeak[+T] {
//  self =>
//
//  def generate:T
//
//  def map[U](f: T => U): GeneratorWeak[U] = {
//    new GeneratorWeak[U] {
//      override def generate: U = f(self.generate)
//    }
//  }
//
//  def flatMap[U](f: T => GeneratorWeak[U]): GeneratorWeak[U] = new GeneratorWeak[U] {
//    def generate: U = f(self.generate).generate
//  }
//}
//
//val iterators = new GeneratorWeak[Int] {
//  val rand = new java.util.Random()
//
//  def generate: Int = rand.nextInt()
//}
//
//iterators.generate
//
//val booleans = iterators map (x => x > 0)
//
//val booleans2 = for(i <- iterators) yield i > 0
//
//def pairs[T, U](t: GeneratorWeak[T], u: GeneratorWeak[U]) = t flatMap { x =>
//  u map { u => (x, u)}
//}
//
//def single[T](x:T) = new GeneratorWeak[T] {
//  override def generate: T = x
//}
//
//def choose[T](lo: Int, hi: Int) = {
//  for (x <- iterators) yield lo + x % (hi - lo)
//}
//
//
//def oneOf[T](xs: T*): GeneratorWeak[T] = {
//  for (x <- choose(0, xs.length)) yield xs(x)
//}
//
//val emptyList = single(Nil)
//
//val nonEmptyList = for {
//  head <- iterators
//  tail <- lists
//} yield head :: tail
//
//def lists: GeneratorWeak[List[Int]] = {
//  for {
//    isEmpty <- booleans
//    list <- if (isEmpty) emptyList else nonEmptyList
//  } yield list
//}
//
//trait Tree
//
//case class Inner(left: Tree, right: Tree) extends Tree
//case class Leaf(x: Int) extends Tree
//
//
//def leafs: GeneratorWeak[Leaf] = for (x <- iterators) yield Leaf(x)
//def inners: GeneratorWeak[Inner] = for {
//  lNode <- trees
//  rNode <- trees
//} yield Inner(lNode, rNode)
//
//def trees: GeneratorWeak[Tree] = {
//  for {
//    isLeaf <- booleans
//    tree <- if (isLeaf) leafs else inners
//  }  yield tree
//}
//
//trees.generate
//trees.generate
//
//
//def f:Int => Double = ((x:Int) => x * x).andThen((x:Int) => math.sqrt(x.toDouble))
//
//f(4)
//
//
//val o: Option[String] = Some("Ala ma kota")
//
//o.flatMap(Some(_))
//
//
//def streamRange(lo: Int, hi: Int): Stream[Int] = {
//
//  print(lo+" ")
//
//  if (lo >= hi) Stream.empty
//
//  else Stream.cons(lo, streamRange(lo + 1, hi))
//
//}
//
//
//val l4 = streamRange(1,10).take(3).toList
//

//// 01 Functions and patterns
//
//
//abstract class JSON
//
//case class JSeq (elems: List[JSON]) extends JSON
//case class JObj (bindings: Map[String, JSON]) extends JSON
//case class JNum (num: Double) extends JSON
//case class JStr (str: String) extends JSON
//case class JBool(b: Boolean) extends JSON
//case object JNull extends JSON
//
///*
//{
//  "firstName" : "John",
//  "lastName" : "Smith",
//  "address": {
//    "streetAddress": "21 2nd Street",
//    "state": "NY",
//    "postalCode": 10021
//  },
//  "phoneNumbers": [
//    { "type": "home", "number": "212 555-1234" },
//    { "type": "fax", "number": "646 555-4567" }
//  ]
//}
//*/
///*
//
//val data = JObj(Map(
//  "firstName" -> JStr("John"),
//  "lastName" -> JStr("Smith"),
//  "address" -> JObj(Map(
//    "streetAddress" -> JStr("21 2nd Street"),
//    "state" -> JStr("NY"),
//    "postalCode" -> JNum(10021)
//  )),
//  "phoneNumbers" -> JSeq(List(
//    JObj(Map(
//      "type" -> JStr("home"), "number" -> JStr("212 555-1234")
//    )),
//    JObj(Map(
//      "type" -> JStr("fax"), "number" -> JStr("646 555-4567")
//    ))
//  ))
//))
//
//def show(json: JSON): String = json match {
//  case JSeq(jlist) => s"[ ${jlist.map {elem => show(elem) }.mkString(",")} ]"
//  case JObj(bindings) =>
//    bindings.map{binding => s"${binding._1} => ${binding._2}"}.mkString("{", ",","}")
//  case JNum(num) => num.toString
//  case JStr(str) => str.toString
//  case JBool(b) => b.toString
//  case JNull => "null"
//}
//
//show(data)
//*/
//
//val f: PartialFunction[List[Int], String] = {
//  case Nil => "one"
//  case x :: rest => rest match {
//    case Nil => "two"
//  }
//}
//
//f.isDefinedAt(List(1,2,3))
////f(List(1,2,3))
//
//
//def map(l: List[Int], f: Int => Int): List[Int] = l match {
//  case Nil => Nil
//  case x :: rest => f(x) :: map(rest, f)
//}
//
//def flatMap(l: List[List[Int]], f: Int => Int): List[Int] = l match {
//  case Nil => Nil
//  case x :: rest => map(x, f) ++ flatMap(rest, f)
//}
//
//def filter(l: List[Int], p: Int => Boolean): List[Int] = l match {
//  case Nil => Nil
//  case x :: rest =>
//    if(p(x)) x :: filter(rest, p)
//    else filter(rest, p)
//}
//
//map(List(1,2,3), x => x * x)
//
//flatMap(List(List(), List(1,2,3), List(2,5)), x => x * x)
//
//filter(List(1,2,3,4,5,6), x => x > 3)
//
//
//for {
//  i <- 1 to 100
//  j <- 1 to i
//  if (i + j >= 199)
//} yield (i, j)
//
//(1 to 100) flatMap { i =>
//  (1 to i)
//    .filter(j => i + j >= 199)
//    .map (j => (i, j))
//}
//
//case class Book(title: String, authors: List[String])
//
//val books: List[Book] = List(
//  Book(title = "Structure and Interpretation of Computer Programs",
//authors = List("Abelson, Harald", "Sussman, Gerald J.")),
//Book(title = "Introduction to Functional Programming",
//authors = List("Bird, Richard", "Wadler, Phil")),
//Book(title = "Effective Java",
//  authors = List("Bloch, Joshua")),
//Book(title = "Effective Java 2",
//  authors = List("Bloch, Joshua")),
//Book(title = "Java Puzzlers",
//authors = List("Bloch, Joshua", "Gafter, Neal")),
//Book(title = "Programming in Scala",
//authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
//
//
//// books with at least one author with a surname Bird
//for {
//  book <- books
//  author <- book.authors
//  if author.startsWith("Bird,")
//} yield book
//
//books.flatMap{book =>
//  book.authors
//    .withFilter(author => author.startsWith("Bird,"))
//    .map(a => a)
//}
//
//// books with at least 3 authors
//for {
//  book <- books
//  if book.authors.length > 2
//} yield book
//
//// find name of the authors who have written at least 2 books
//for {
//  book1 <- books
//  book2 <- books
//  if book1.title < book2.title
//  author1 <- book1.authors
//  author2 <- book2.authors
//  if author1 == author2
//} yield author1
//
//
///*def mapFun[T, U](xs: List[T], f: T => U): List[U] = {
//  for(x <- xs) yield f(x)
//}
//
//def flatMap[T, U](xs: List[T], f: T => List[U]): List[U] = {
//  for(x <- xs; a <- f(x)) yield a
//}
//
//def filter[T](xs: List[T], p: T => Boolean): List[T] = {
//  for(x <- xs if p(x)) yield x
//}*/
//
//
//val l1: List[Int] = (1 to 100).toList
//for (x <- l1.withFilter(x => x > 97)) yield x




