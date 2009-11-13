package com.osinka.mongodb

import com.mongodb.DBObject

trait Serializer[T] {
    def in(obj: T): DBObject
    def out(dbo: DBObject): Option[T]
    def mirror(x: T)(dbo: DBObject): T = x
}