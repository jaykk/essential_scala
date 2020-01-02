trait Feline {
  def colour: String
  def sound: String
}

case class Cat(
              colour: String,
              food: String
              ) extends Feline {
  val sound = "meow"
}

case class Tigers(
                   colour: String,
                 ) extends Feline {
  val sound = "roar"
}

case class Lions(
                   colour: String,
                   maneSize: Int
                 ) extends Feline {
  val sound = "roar"
}

case class Panthers(
                  colour: String,
                ) extends Feline {
  val sound = "roar"
}

trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

case class square(sideLength: Double) extends Shape {
  val sides = 4
  val perimeter = sides * sideLength
  val area = sideLength * sideLength
}

case class rect(width: Double, height: Double) extends Shape{
  val sides = 4
  val perimeter = 2 * width + 2 * height
  val area = width * height
}

case class circle(radius: Double) extends Shape {
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * radius * radius
}

sealed trait Rectangular extends Shape {
  def width: Double
  def height: Double
  def sides = 4
  def perimeter = 2 * width + 2 * height
  def area = width * height
}

case class rect(width: Double, height: Double) extends Rectangular
case class square(width: Double) extends Rectangular {
  val height = width
}

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

sealed trait Rectangular extends Shape {
  def width: Double
  def height: Double
  def sides = 4
  def perimeter = 2 * width + 2 * height
  def area = width * height
}

case class rect(width: Double, height: Double) extends Rectangular
case class square(width: Double) extends Rectangular {
  val height = width
}
case class Circle(radius: Double) extends Shape{
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * radius * radius
}

object Draw {
  def apply(s: Shape): Unit =
    s match {
      case Circle(radius) => println("A circle with radius " + radius)
      case rect(width, height) => println("A rectangle with width " + width + " and height " + height)
      case square(width) => println("A square with side" + width)
    }
}

sealed trait Colour {
  def red: Int
  def green: Int
  def blue: Int
}

final case class Red() extends Colour {
  val red = 255
  val green = 0
  val blue = 0
}

final case class Yellow() extends Colour {
  val red = 255
  val green = 255
  val blue = 0
}

final case class CustomColour(red: Int, green: Int, blue: Int) extends Colour {}

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def colour: Colour
}

case class Circle(radius: Double, colour: Colour) extends Shape{
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * radius * radius
}

object draw {
  def apply(s: Shape): Unit =
    s match {
      case Circle(radius, Yellow()) => println("A yellow circle with radius " + radius)
      case Circle(radius, aColour) =>
        if (aColour.red + aColour.green + aColour.blue > 200)
          println("A ligth circle with radius " + radius)
        else
          println("A dark circle with radius " + radius)
    }
}

draw(new Circle(5, new Yellow()))
draw(new Circle(7, new CustomColour(3,5,8)))

sealed trait DivisionResult {
  def result: Any
}

case class Finite(result: Int) extends DivisionResult {}
case class Infinite() extends DivisionResult {
  val result = "Infinite"
}

object divide {
  def apply(a: Int, b: Int): DivisionResult = if (b == 0)
    Infinite()
  else
    Finite(a/b)
}

val d = divide(3, 4)
d match {
  case Infinite() => println("Call to divide by zero")
  case Finite(_) => println("Divided correctly, result: " + d)
}
val d1 = divide(4, 4)
d1 match {
  case Infinite() => println("Call to divide by zero")
  case Finite(_) => println("Divided correctly, result: " + d1)
}
val d2 = divide(4, 0)
d2 match {
  case Infinite() => println("Call to divide by zero")
  case Finite(_) => println("Divided correctly, result: " + d2)
}

//sealed trait TrafficLight
//final case class Red() extends TrafficLight
//final case class Yellow() extends TrafficLight
//final case class Green() extends TrafficLight

// Answer:
sealed trait TrafficLight
case object Red extends TrafficLight
case object Yellow extends TrafficLight
case object Green extends TrafficLight

//sealed trait Result
//case object Succeed extends Result
//case object Failed extends Result
//case class Calculation(result: Result)

//Answer:
sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(reason: String) extends Calculation

sealed trait Source
case object Well extends Source
case object Spring extends Source
case object Tap extends Source
case class BottledWater(size: Int, source: Source, carbonated: Boolean)

//sealed trait TrafficLight2 {
//  def next: TrafficLight2 = {
//    this match {
//      case Red2() => Green2()
//      case Yellow2() => Red2()
//      case Green2() => Yellow2()
//    }
//  }
//}
//final case class Red2() extends TrafficLight2
//final case class Yellow2() extends TrafficLight2
//final case class Green2() extends TrafficLight2

object Calculator {
  def +(s: Success, c: Int): Calculation = {
    Success(s.result + c)
  }
  def -(s: Success, c: Int): Calculation = {
    Success(s.result - c)
  }
  def +(s: Failure, c: Int): Calculation = {
    Failure(s.reason)
  }
  def -(s: Failure, c: Int): Calculation = {
    Failure(s.reason)
  }
}
assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

object Calculator {
  def +(s: Calculation, c: Int): Calculation = {
    s match {
      case Success(result) => Success(result + c)
      case Failure(reason) => Failure(reason)
    }
  }
  def -(s: Calculation, c: Int): Calculation = {
    s match {
      case Success(result) => Success(result - c)
      case Failure(reason) => Failure(reason)
    }
  }
}
assert(Calculator.+(Success(1), 1) == Success(2))
assert(Calculator.-(Success(1), 1) == Success(0))
assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))


