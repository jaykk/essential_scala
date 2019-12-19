object calc{
  def square(a: Double): Double = a * a
}

calc.square(4.0)

object calc2{
  def square(a: Double): Double = a * a
  def square(a: Int): Int = a * a
}

calc2.square(4.0)
calc2.square(4)

object argh {
  def a = {
    println("a")
    1
  }
  val b = {
    println("b")
    a+2
  }
  def c = {
    println("c")
    a
    b + "c" }
}

// expected output:
// c
// a
// b
// a
// b
// a
// a
// res: type err

// actual output:
// b
// a
// c
// a
// a
// res3: String = 3c31
argh.c + argh.b + argh.a
println("Need a println to show above")

object person {
  def firstName = "Fred"
  def lastName = "Mercury"
}

object alien {
  def greet(p: person.type) = {
    println("Greetings, " + p.firstName)
  }
}

val p = person
val a = alien
a.greet(p)

def square(in : Double) =
  in * in

assert(square(2.0) == 4.0)
assert(square(3.0) == 9.0)
assert(square(-2.0) == 4.0)

if(1 < 2) "Yes" else "No"

if (1 > 2) "alien" else "predator"

if(1 > 2) "alien" else 2001

if(false) "hello"
