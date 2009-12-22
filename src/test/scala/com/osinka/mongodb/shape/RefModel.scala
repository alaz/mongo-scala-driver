package com.osinka.mongodb.shape

import com.mongodb.DBObject
import com.osinka.mongodb._

class RefModel {
    var message: String = _
    var user: CaseUser = _
}

class RefModelShape(coll: MongoCollection[CaseUser]) extends ObjectShape[RefModel] { shape =>
    lazy val message = Field.scalar("message", _.message, (x: RefModel, v: String) => x.message = v)

    object user extends MongoScalar[CaseUser] with RefContent[CaseUser] with CaseUserIn[RefModelShape] {
        override val mongoFieldName = "user"
        override val coll: MongoCollection[CaseUser] = shape.coll
        override val rep = shape.Represented.by(_.user, Some( (x: RefModel, v: CaseUser) => x.user = v) )
    }

    lazy val * = List(message, user)
    override def factory(dbo: DBObject) = Some(new RefModel)
}