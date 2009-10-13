package com.osinka.mongodb.shape

import com.mongodb._

// case class Model with constant field, its extractor and factory method
case class CaseUser(val name: String) extends MongoObject

trait CaseUserFieldsIn[T] extends ShapeFields[CaseUser, T] {
    object name extends Scalar[String]("name", _.name) with Functional[String]
}

object CaseUser extends Shape[CaseUser] with CaseUserFieldsIn[CaseUser] {
    override lazy val * = name :: super.*
    override def factory(dbo: DBObject): Option[CaseUser] = for {val name(n) <- Some(dbo)} yield new CaseUser(n)
}

// ordinary class model with variable and updatable field
class OrdUser extends MongoObject {
    var name: String = _
}
object OrdUser extends AbstractShape[OrdUser] {
    object name extends Scalar[String]("name", _.name) with Updatable[String] {
        override def update(x: OrdUser, name: String): Unit = x.name = name
    }

    override lazy val * = name :: super.*
}

// object holder for serializer tests
case class Holder[T](var value: T)

class TSerializer[T](val f: () => Holder[T]) extends DBObjectShape[Holder[T]] with ShapeFunctional[Holder[T]] {
    object i extends Scalar[T]("i", _.value) with Updatable[T] {
        override def update(x: Holder[T], v: T): Unit = x.value = v
    }

    override lazy val * = i :: Nil
    override def factory(dbo: DBObject): Option[Holder[T]] = Some(f())
}
