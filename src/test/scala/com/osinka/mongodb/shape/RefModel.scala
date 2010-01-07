package com.osinka.mongodb.shape

import com.mongodb.{DB,DBObject}
import com.osinka.mongodb._

class RefModel(val message: String, val user: CaseUser)

class RefModelShape(val db: DB, val usersCollName: String) extends ObjectShape[RefModel] { shape =>
    lazy val message = Field.scalar("message", _.message)

    lazy val user = Field.ref("user", CaseUser collection db.getCollection(usersCollName), _.user)
//
// same as
//
//    object user extends MongoScalar[CaseUser] with RefContent[CaseUser] with Functional[CaseUser] {
//        override val mongoFieldName = "user"
//        override lazy val coll: MongoCollection[CaseUser] = CaseUser collection db.getCollection(usersCollName)
//        override val rep = shape.Represented.by(_.user, None)
//    }

    lazy val * = List(message, user)
    override def factory(dbo: DBObject) =
        for {message(m) <- Some(dbo)
             user(u) <- Some(dbo)} yield new RefModel(m, u)
}