package com.osinka.mongodb.shape

import com.mongodb.DBObject
import com.osinka.mongodb._

class RefModel(val message: String, val user: CaseUser)

class RefModelShape(coll: MongoCollection[CaseUser]) extends ObjectShape[RefModel] { shape =>
    lazy val message = Field.scalar("message", _.message)

    object user extends MongoScalar[CaseUser] with RefContent[CaseUser] with CaseUserIn[RefModelShape] with Functional[CaseUser] {
        override val mongoFieldName = "user"
        override val coll: MongoCollection[CaseUser] = shape.coll
        override val rep = shape.Represented.by(_.user, None)
    }

    lazy val * = List(message, user)
    override def factory(dbo: DBObject) =
        for {message(m) <- Some(dbo)
             user(u) <- Some(dbo)} yield new RefModel(m, u)
}