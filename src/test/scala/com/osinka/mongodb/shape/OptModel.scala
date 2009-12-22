package com.osinka.mongodb.shape

import com.mongodb._

class OptModel(val id: Int, val description: Option[String]) {
    var comment: Option[String] = None
}

object OptModel extends ObjectShape[OptModel] {
    lazy val id = Field.scalar("id", _.id)

    // Hurray! Option[A] field!
    lazy val description = Field.optional("description", _.description)
    // OR much longer:
     
    object description3 extends OptionalField[String]("description", _.description, None) with Functional[String]

    lazy val comment = Field.optional("comment", _.comment, (obj: OptModel, v: Option[String]) => obj.comment = v)

    override def * = List(id, description, comment)

    override def factory(dbo: DBObject) =
        for {id(i) <- Some(dbo)}
        yield new OptModel(i, description from dbo)
}