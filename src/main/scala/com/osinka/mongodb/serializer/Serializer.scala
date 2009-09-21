package com.osinka.mongodb.serializer

import com.mongodb._
import com.osinka.mongodb._

trait Serializer[T] {
    protected val in: PartialFunction[T, DBObject]
    protected val out: PartialFunction[DBObject, T]
    protected def mirror(x: T)(dbo: DBObject): T
}

trait PlainDBOSerializer extends Serializer[DBObject] {
    override val in: PartialFunction[DBObject, DBObject] = { case x => x }
    override val out: PartialFunction[DBObject, DBObject] = { case x => x }
    override def mirror(x: DBObject)(dbo: DBObject) = x
}