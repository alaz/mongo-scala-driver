package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

object Helper {
    private[mongodb] def tryo[T](obj: T): Option[T] =
        if (null == obj) None
        else Some(obj)

    private[mongodb] def emptyDBO: DBObject = new BasicDBObject

    private[mongodb] def merge(dbo1: DBObject, dbo2: DBObject) = {
        val dbo = emptyDBO
        dbo putAll dbo1
        dbo putAll dbo2
        dbo
    }
}