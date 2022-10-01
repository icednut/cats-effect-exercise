package io.icednut.cats.effect.exercise.part1recap

import io.icednut.cats.effect.exercise.part1recap.ContextualAbstractionsScala2.Person

object ContextualAbstractionsScala2 {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class PersonExt(name: String) {
    def greet: String = Person(name).greet
  }

  val greeting = "Peter".greet

  import scala.concurrent.duration._
  val oneSecond = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount: Int = 10
  val twelve = increment(2)

  def times(x: Int)(implicit factor: Int) = x * factor
  val aHundred = times(10)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def convert2Json[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String = s"{\"name\": \"${person.name}\"}"
  }

  val davidsJson = convert2Json(Person("David"))

  // implicit defs
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}]"
    }

  val personsJson = convert2Json(List(Person("Alice"), Person("Peter"), Person("Tom"), Person("Will")))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow: String = s"$name is meowing"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)
  val aCat: Cat = "Garfield" // stirng2Cat("Garfield")
  val garfieldMeowing = "Garfield".meow

  def main(args: Array[String]): Unit = {
    println(davidsJson)
    println(personsJson)

  }
}

object TypeclassesScala2 {
  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances
  implicit object stringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = s"\"${value}\""
  }
  implicit object intSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }
  implicit object personSerializer extends JSONSerializer[Person] {
    override def toJson(person: Person): String = s"{\"name\": \"${person.name}\", \"age\": \"${person.age}\"}"
  }

  // part 3 - offer some API
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)
  def convertToListJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(convertToJson(_)).mkString("[", ",", "]")

  // part 4 - an extension methods
  object JsonSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  // TODO 그냥 코딩하다가 타입 클래스를 통해 리팩토링 하는 모습을 보여주는게 어떨까? (포스팅 아이템)

  def main(args: Array[String]): Unit = {
    import JsonSyntax._

    println(convertToListJson(List(Person("Alice", 23), Person("Peter", 23), Person("Tom", 23), Person("Will", 23))))
    println(Person("Alice", 23).toJson)
  }
}