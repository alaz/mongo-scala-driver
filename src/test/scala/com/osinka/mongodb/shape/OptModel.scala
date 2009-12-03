package com.osinka.mongodb.shape

import com.mongodb._

/*
class OptModel(val id: Int, val description: Option[String]) {
    var comment: String = OptModel.DefaultComment
}

object OptModel extends ObjectShape[OptModel] {
    val DefaultComment = "default"

    lazy val id = Scalar("id", _.id)
    lazy val description = Scalar("description", _.description)
    object comment extends Scalar[String]("comment", _.comment) with Updatable[String] with Optional[String] {
        override def update(obj: OptModel, v: String) { obj.comment = v }
    }

    override def * = id :: description :: comment :: Nil

    override def factory(dbo: DBObject) =
        for {id(i) <- Some(dbo)
             description(descr) <- Some(dbo)}
        yield new OptModel(i, descr)
}
*/