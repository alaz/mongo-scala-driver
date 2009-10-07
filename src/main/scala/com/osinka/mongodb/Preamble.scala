package com.osinka.mongodb

import com.mongodb.DBCollection
import serializer.Conversions
import shape._

object Preamble extends Conversions {
    implicit def collToScala(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
        def asScala = new DBObjectCollection(coll)
    }

    implicit def queryToColl(q: Query) = new {
//        def in(coll: DBObjectCollection) = coll.applied(q)
        def in[T, Self <: QueriedCollection[T, Self]](coll: QueriedCollection[T, Self]): Self = coll.applied(q)
    }

    implicit def WrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying
}