package com.osinka.mongodb

import scala.reflect.Manifest
import com.mongodb.DBCollection

object Preamble {
    def wrap(coll: DBCollection) = new DBObjectCollection(coll)

    implicit def dbCollToWrapper(coll: DBCollection) = new {
        def of[T] = new TypedDBOCollection[T](coll)
        def asScala = new DBObjectCollection(coll)
    }

    implicit def WrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying
}