package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb._

// case class Model with constant field, its extractor and factory method
case class CaseUser(val name: String) extends MongoObject

trait CaseUserIn[T] extends ObjectIn[CaseUser, T] {
    object name extends ScalarField[String]("name", _.name, None) with Functional[String]
    override lazy val * = name :: Nil
    override def factory(dbo: DBObject): Option[CaseUser] = for {val name(n) <- Some(dbo)} yield new CaseUser(n)
}

object CaseUser extends MongoObjectShape[CaseUser] with CaseUserIn[CaseUser]

// ordinary class model with variable and updatable field
class OrdUser extends MongoObject {
    var name: String = _
}
object OrdUser extends MongoObjectShape[OrdUser] {
    override def factory(dbo: DBObject) = Some(new OrdUser)

    lazy val name = Field.scalar("name",
           (u: OrdUser) => u.name,
           (u: OrdUser, n: String) => u.name = n)

    override lazy val * = name :: Nil
}

// object holder for serializer tests
case class Holder[T](var value: T)

class TSerializer[T](val f: () => Holder[T]) extends ObjectShape[Holder[T]] with FunctionalShape[Holder[T]] {
    lazy val i = Field.scalar("i", (x: Holder[T]) => x.value, (x: Holder[T], v: T) => x.value = v)

    override lazy val * = List(i)
    override def factory(dbo: DBObject): Option[Holder[T]] = Some(f())
}
