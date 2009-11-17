package com.osinka.mongodb.shape

import com.mongodb._

class ComplexType(val user: CaseUser) extends MongoObject

object ComplexType extends MongoObjectShape[ComplexType] {
    object user extends Embedded[CaseUser]("user", CaseUser, _.user) with Functional[CaseUser] with CaseUserFieldsIn[ComplexType]

    override lazy val * = user :: super.*
    override def factory(dbo: DBObject): Option[ComplexType] = for {val user(u) <- Some(dbo)} yield new ComplexType(u)
}