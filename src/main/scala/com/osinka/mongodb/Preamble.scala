package com.osinka.mongodb

import com.mongodb.DBCollection
import serializer.Conversions
import shape._

object Preamble extends Conversions {
    implicit def dbCollToWrapper(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
        def asScala = new DBObjectCollection(coll)
    }

    implicit def WrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying
}