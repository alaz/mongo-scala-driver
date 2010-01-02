package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb._

class ComplexType(val user: CaseUser, val messageCount: Int) extends MongoObject

object ComplexType extends MongoObjectShape[ComplexType] with FunctionalShape[ComplexType] {
    object user extends EmbeddedField[CaseUser]("user", _.user, None) with Functional[CaseUser] with CaseUserIn[ComplexType]
    object messageCount extends ScalarField[Int]("msgs", _.messageCount, None) with Functional[Int]

    override lazy val * = user :: messageCount :: Nil
    override def factory(dbo: DBObject): Option[ComplexType] =
        for {user(u) <- Some(dbo)
             messageCount(x) <- Some(dbo)}
        yield new ComplexType(u, x)
}