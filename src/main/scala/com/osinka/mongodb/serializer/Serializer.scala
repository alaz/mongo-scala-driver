package com.osinka.mongodb.serializer

import com.mongodb._
import com.osinka.mongodb._

trait Serializer[T] {
    protected def in(obj: T): DBObject
    protected def out(dbo: DBObject): Option[T]
    protected def mirror(x: T)(dbo: DBObject): T = x
}

trait PlainDBOSerializer extends Serializer[DBObject] {
    override def in(obj: DBObject) = obj
    override def out(dbo: DBObject) = Some(dbo)
    override def mirror(x: DBObject)(dbo: DBObject) = dbo
}