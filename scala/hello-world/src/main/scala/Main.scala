@main def hello(): Unit =
  println("Hello world!")
  println(x + "LOL")
  print("Interpolated stuff I was compiled by Scala 3. :)")
  val i = 42
  println(
    if (i == 42) if (i == 43) "Yesn't" else "Non't"
    else "Hahaha")

  val xx: Int = {
    println("This is a code block")
    4
  }
  println(add(2, 3))
  val unit: Unit = println(factorial(4))
  println(s"Unit is $unit")
  val dog = new Dog("Lassie")
  dog.bark()
  println(dog.age(20))
  println(dog.name)
  val dog1 = new MutantDog
  val cat: Animal = new Cat
  cat.eat()
  val animal = new Animal:
    override def eat(): Unit = print("In-situ animal")
  animal.eat()
  object FlowTypeNode {
    val x = 53
    def method(): Unit = println("FlowTypeNode")
    def apply(x: Int): Int = 43
  }

  println(FlowTypeNode(30))
  val person = Person("ABCD", 20)
  println(person.name)
  val ints: List[Int] = List(1, 2, 3)
  val value = ints.head
  val sayHi: String => String = (v1: String) => s"Hi, $v1"
  val basicAdd: (Int, Int) => Int = (v1: Int, v2: Int) => v1 + v2
  def add2(v1: Int)(v2: Int): Int = v1 + v2

  val add2To = add2(2)
  println(add2To(30))

  println(sayHi("Mojo"))
  val set1 = Set(1, 2, 3)
  val set2 = Set(1, 2)
  val diff = set1.diff(set2)
  println(diff)
  val c: (String, Int, String) = ("A", 1, "ddd")
  val phoneBook = Map("a" -> 10000, "b" -> 20000)
  println(phoneBook("a"))


  val forty = 40
  val str = forty match
    case 1 => "First"
    case 2 => "Second"
    case 3 => "Third"
    case _ => s"${forty}th"

    println(str)
    println(person match
      case Person(n, a) => s"$n is $a years old"
    )

val x: String = "Hello world"


class Dog(val name: String) {
  def bark(): Unit = println("WOOF")
  def age(humanAge: Int): Int = humanAge * 3
}

class MutantDog extends Dog("ABCD") {
}

trait Animal {
  def eat(): Unit
}

class Cat extends Animal {
  override def eat(): Unit = println("Meow")
}

def add(x: Int, y: Int): Int = x + y
def factorial(x: Int): Int =
  if (x == 1) 1
  else x * factorial(x - 1)

case class Person(name: String, age: Int)
