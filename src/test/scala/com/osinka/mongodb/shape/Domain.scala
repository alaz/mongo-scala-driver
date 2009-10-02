package com.osinka.mongodb.shape

import com.mongodb._

case class CaseUser(val name: String) extends MongoObject

object CaseUser extends Shape[CaseUser] {
    object name extends scalar[String]("name", _.name) with Functional[String]
    
    override val * = name :: super.*
    override def factory(dbo: DBObject): Option[CaseUser] = for {val name(n) <- Some(dbo)} yield new CaseUser(n)
}

class OrdUser extends MongoObject {
    var name: String = _
}
object OrdUser extends Shape[OrdUser] {
    object name extends scalar[String]("name", _.name) with Updatable[String] {
        override def update(x: OrdUser, name: String): Unit = x.name = name
    }

    override val * = name :: super.*
}

class ComplexType(val user: CaseUser) extends MongoObject

object ComplexType extends Shape[ComplexType] {
    object user extends nested[CaseUser]("user", CaseUser, _.user) with Functional[CaseUser]

    override val * = user :: super.*
    override def factory(dbo: DBObject): Option[ComplexType] = for {val user(u) <- Some(dbo)} yield new ComplexType(u)
}

case class Holder[T](var value: T)

class TSerializer[T](val f: () => Holder[T]) extends DBObjectShape[Holder[T]] with ShapeFunctional[Holder[T]] {
    object i extends scalar[T]("i", _.value) with Updatable[T] {
        override def update(x: Holder[T], v: T): Unit = x.value = v
    }

    override val * = i :: Nil
    override def factory(dbo: DBObject): Option[Holder[T]] = Some(f())
}
