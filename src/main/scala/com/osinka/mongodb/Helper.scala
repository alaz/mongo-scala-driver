package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

object Helper {
    private[mongodb] def tryo[T](obj: T): Option[T] =
        if (null == obj) None
        else Some(obj)

    private[mongodb] def emptyDBO: DBObject = new BasicDBObject
}