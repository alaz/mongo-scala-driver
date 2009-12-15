package com.osinka.mongodb.shape

import com.mongodb._

class OptModel(val id: Int, val description: Option[String]) {
    var comment: String = OptModel.DefaultComment
}

object OptModel extends ObjectShape[OptModel] {
    val DefaultComment = "default"

    lazy val id = Scalar("id", _.id)
    object description extends Scalar[Option[String]]("description", _.description) with Functional[Option[String]] {
        override val serializer = opt[String] + defl[String]
    }
    object comment extends Scalar[String]("comment", _.comment) with Updatable[String] with Optional[String] {
        override def update(obj: OptModel, v: String) { obj.comment = v }
    }

    override def * = List(id, description, comment)

    override def factory(dbo: DBObject) =
        for {id(i) <- Some(dbo)
             description(descr) <- Some(dbo)}
        yield new OptModel(i, descr)
}