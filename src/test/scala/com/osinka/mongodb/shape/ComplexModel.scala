package com.osinka.mongodb.shape

import com.mongodb._

class ComplexType(val user: CaseUser, val messageCount: Int) extends MongoObject

object ComplexType extends MongoObjectShape[ComplexType] with FunctionalShape[ComplexType] {
    object user extends Embedded[CaseUser]("user", _.user) with Functional[CaseUser] with CaseUserIn[ComplexType]
    object messageCount extends Scalar[Int]("msgs", _.messageCount) with Functional[Int]

    override lazy val * = user :: messageCount :: Nil
    override def factory(dbo: DBObject): Option[ComplexType] =
        for {user(u) <- Some(dbo)
             messageCount(x) <- Some(dbo)}
        yield new ComplexType(u, x)
}