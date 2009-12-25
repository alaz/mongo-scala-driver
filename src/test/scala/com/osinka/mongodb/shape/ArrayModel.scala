package com.osinka.mongodb.shape

import com.mongodb.{DB,DBObject}
import com.osinka.mongodb._

object ArrayOfInt {
    class ArrayModel(val id: Int) {
        var messages: List[Int] = Nil
    }

    object ArrayModel extends ObjectShape[ArrayModel] { shape =>
        lazy val id = Field.scalar("id", _.id)

        object messages extends MongoArray[Int] with ScalarContent[Int] {
            override val mongoFieldName = "messages"
            override val rep = Represented.by[Seq[Int]](_.messages, Some( (x: ArrayModel, l: Seq[Int]) => x.messages = l.toList ))
        }

        lazy val * = List(id, messages)
        override def factory(dbo: DBObject) = for {id(i) <- Some(dbo)} yield new ArrayModel(i)
    }
}

object ArrayOfEmbedded {
    class ArrayModel(val id: Int) {
        var users: List[CaseUser] = Nil
    }

    object ArrayModel extends ObjectShape[ArrayModel] { shape =>
        lazy val id = Field.scalar("id", _.id)

        object users extends MongoArray[CaseUser] with EmbeddedContent[CaseUser] with CaseUserIn[ArrayModel] {
            override val mongoFieldName = "users"
            override val rep = shape.Represented.by[Seq[CaseUser]]( _.users, Some( (x: ArrayModel, l: Seq[CaseUser]) => x.users = l.toList ))
        }

        lazy val * = List(id, users)
        override def factory(dbo: DBObject) = for {id(i) <- Some(dbo)} yield new ArrayModel(i)
    }
}

object ArrayOfRef {
    class ArrayModel(val id: Int) {
        var users: List[CaseUser] = Nil
    }

    class ArrayModelShape(val db: DB, val usersCollName: String) extends ObjectShape[ArrayModel] { shape =>
        lazy val id = Field.scalar("id", _.id)

        object users extends MongoArray[CaseUser] with RefContent[CaseUser] {
            override val mongoFieldName = "users"
            override lazy val coll: MongoCollection[CaseUser] = CaseUser collection db.getCollection(usersCollName)
            override val rep = shape.Represented.by[Seq[CaseUser]]( _.users, Some( (x: ArrayModel, l: Seq[CaseUser]) => x.users = l.toList ))
        }

        lazy val * = List(id, users)
        override def factory(dbo: DBObject) = for {id(i) <- Some(dbo)} yield new ArrayModel(i)
    }
}