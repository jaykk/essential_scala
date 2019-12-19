class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = firstName + " " + lastName
}

class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(d: Director): Boolean = d == director
}

val eastwood = new Director("Clint", "Eastwood", 1930)
val mcTiernan = new Director("John", "McTiernan", 1951)
val nolan = new Director("Christopher", "Nolan", 1970)
val someBody = new Director("Just", "Some Body", 1990)
val memento = new Film("Memento", 2000, 8.5, nolan)
val inception = new Film("Inception", 2010, 8.8, nolan)

// 3.1.6.4

class Counter(val count: Int = 1) {
  def inc: Counter = new Counter(count + 1)
  def dec: Counter = new Counter(count - 1)
}

new Counter(10).inc.dec.inc.inc.count

class Counter(val count: Int = 1) {
  def dec: Counter = dec()
  def inc: Counter = inc()
  def inc(amt: Int = 1): Counter = new Counter(count + 1)
  def dec(amt: Int = 1): Counter = new Counter(count - 1)
}

new Counter(10).inc.dec.inc.inc.count

class Adder(amount: Int) {
  def add(in: Int) = in + amount
}

class Counter(val count: Int = 1) {
  def dec: Counter = dec()
  def inc: Counter = inc()
  def inc(amt: Int = 1): Counter = new Counter(count + 1)
  def dec(amt: Int = 1): Counter = new Counter(count - 1)
  def adjust(a: Adder): Counter = new Counter(a.add(count))
}

new Counter(10).adjust(new Adder(5)).count

class Adder(amount: Int) {
  def apply(in: Int): Int = in + amount
}

val add3 = new Adder(3)
add3.apply(2)
add3(4)

class Person(val firstName: String, val lastName: String) {}

object Person {
  def apply(fullName: String): Person = {
    val names = fullName.split(" ")
    new Person(firstName = names(0), lastName = names(1))
  }
}

Person("Bill Murray").firstName

class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = firstName + " " + lastName
}
object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): Director = new Director(firstName = firstName, lastName = lastName, yearOfBirth = yearOfBirth)
  def older(d1: Director, d2: Director) = if (d1.yearOfBirth <= d2.yearOfBirth) d1 else d2
}

class Film(val name: String, val yearOfRelease: Int, val imdbRating: Double, val director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(d: Director): Boolean = d == director
}
object Film {
  def apply(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) = new Film(name, yearOfRelease, imdbRating, director)
  def hightRating(f1: Film, f2: Film): Film = if (f1.imdbRating >= f2.imdbRating) f1 else f2
  def oldestDirectorAtTheTime(f1: Film, f2: Film) = if (f1.directorsAge >= f2.directorsAge) f1.director else f2.director
}


val eastwood = new Director("Clint", "Eastwood", 1930)

// Film is a type
// val prestige: Film = bestFilmByChristopherNolan()

// Film is a class so type
// new Film("Last Action Hero", 1993, mcTiernan)

// value
//Film("Last Action Hero", 1993, mcTiernan)

// either?
// could be either? but context implies a value
//Film.newer(highPlainsDrifter, thomasCrownAffair)

// value
//Film.type

case class Cat(name: String, colour: String, food: String) {

}

case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name: String = firstName + " " + lastName
}
object Director {
  def older(d1: Director, d2: Director) = if (d1.yearOfBirth <= d2.yearOfBirth) d1 else d2
}

case class Film(name: String, yearOfRelease: Int, imdbRating: Double, director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(d: Director): Boolean = d == director
}
object Film {
  def hightRating(f1: Film, f2: Film): Film = if (f1.imdbRating >= f2.imdbRating) f1 else f2
  def oldestDirectorAtTheTime(f1: Film, f2: Film) = if (f1.directorsAge >= f2.directorsAge) f1.director else f2.director
}

case class Counter(count: Int = 0) {
  def dec: Counter = dec()
  def inc: Counter = inc()
  def inc(amt: Int = 1): Counter = copy(count + 1)
  def dec(amt: Int = 1): Counter = copy(count - 1)
}

case class Person(firstName: String, lastName: String) {
  def name = firstName + " " + lastName
}
object Person {
  def apply(name: String): Person = {
    val parts = name.split(" ")
    apply(parts(0), parts(1))
  }
}

object ChipShop {
  def willServe(cat: Cat): Boolean =
    cat match {
      case Cat(_, _, "chips") => true
      case _ => false
    }
}

object dad{
  def rate(movie: Film): Double =
    movie match {
      case Film(_, _, _, Director("Clint", "Eastwood", 1930)) => 10.0
      case Film(_, _, _, Director("John", "McTiernan", 1951)) => 7.0
      case _ => 3.0
    }
}
