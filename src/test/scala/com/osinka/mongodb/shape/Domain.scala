package com.osinka.mongodb.shape

case class CaseUser(var name: String) extends MongoObject
object CaseUser extends Shape[CaseUser] {
    override def factory = CaseUser("")

    object name extends scalar[String]("name") {
        def apply(x: CaseUser) = x.name
        def update(x: CaseUser, v: String) { x.name = v }
    }

    override val * = name :: super.*
}

class OrdUser extends MongoObject {
    var name: String = _
}
object OrdUser extends Shape[OrdUser] {
    object name extends scalar[String]("name") {
        def apply(x: OrdUser): String = x.name
        def update(x: OrdUser, v: String): Unit = x.name = v
    }

    override val * = name :: super.*
}

class ComplexType extends MongoObject {
    var user: CaseUser = _
}
object ComplexType extends Shape[ComplexType] {
    object user extends nested[CaseUser, Shape[CaseUser]]("user", CaseUser) {
        def apply(x: ComplexType) = x.user
        def update(x: ComplexType, y: CaseUser) { x.user = y }
    }

    override val * = user :: super.*
}

case class Holder[T](var value: T)

class TSerializer[T](val f: () => Holder[T]) extends DBObjectShape[Holder[T]] {
    object i extends scalar[T]("i") {
        def apply(x: Holder[T]): T = x.value
        def update(x: Holder[T], v: T): Unit = x.value = v
    }

    override val * = i :: Nil
    override def factory = f()
}
