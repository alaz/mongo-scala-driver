package com.osinka.mongodb

import com.mongodb.DBObject

trait Serializer[T] {
    def in(obj: T): DBObject
    def out(dbo: DBObject): Option[T]
    def mirror(x: T)(dbo: DBObject): T = x
}

class PlainDBOSerializer extends Serializer[DBObject] {
    override def in(obj: DBObject) = obj
    override def out(dbo: DBObject) = Some(dbo)
    override def mirror(x: DBObject)(dbo: DBObject) = dbo
}