package com.osinka.mongodb.shape

import com.mongodb.DBObject

case class CaseUser(var name: String) extends MongoObject
object CaseUser extends Shape[CaseUser] {
    override def factory(dbo: DBObject) = CaseUser("")

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