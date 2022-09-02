package chapter1

sealed trait Json

final case class JsObject (get: Map[String, Json]) extends Json
final case class JsString (get: String) extends Json
final case class JsNumber (get: Double) extends Json 
final case object JsNull extends Json

object Json {

  // Use the type class
  def toJson [A] (value: A) (implicit w: JsonWriter[A]): Json =
    w.write(value)
} 

object JsonSyntax {

  // Extend existing types with methods for better ergonomics
  implicit class JsonWriterSyntax [A] (value: A) {

    def toJson (implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

}

// Type class that representes the serialization to JSON
trait JsonWriter [A] {
  
  def write (value: A): Json
}

object JsonWriter {

  def apply [A] (implicit w: JsonWriter[A]) = w

  // Instances of the type class
  implicit def stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit def intWriter: JsonWriter[Int] =
    (value: Int) => JsNumber(value.toDouble)

  implicit def doubleWriter: JsonWriter[Double] =
    JsNumber(_)

  // Instances that depend on other instances
  implicit def optionWriter [A: JsonWriter] = new JsonWriter[Option[A]] {

    def write (option: Option[A]): Json = option match {
      case Some(v) => JsonWriter[A].write(v)
      case None    => JsNull
    }
  }
}

final case class Person (name: String, email: String)

object Person {

  implicit def personWriter: JsonWriter[Person] =
    (person: Person) => JsObject(Map(
      "name"  -> JsString(person.name),
      "email" -> JsString(person.email)
    ))

}

object Main extends App {

  import JsonSyntax._

  val person = Person("Sebastian", "sebastian")
  
  // Example of type class usage
  println(Json.toJson(person))
  println(person.toJson)
  println(Option(person).toJson)
  println(Option.empty[Person].toJson)
}