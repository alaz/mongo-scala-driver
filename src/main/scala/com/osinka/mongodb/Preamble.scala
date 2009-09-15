package com.osinka.mongodb

import scala.reflect.Manifest
import com.mongodb.DBCollection

object Preamble extends Conversions {
    implicit def dbCollToWrapper(coll: DBCollection) = new {
//        def of[T] = new MutableCollection[T](coll)
        def asScala = PlainDBOBuilder.mutable(coll)
    }

    implicit def WrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying
}