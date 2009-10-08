package com.osinka.mongodb

import com.mongodb.DBCollection
import serializer.Conversions
import shape.Implicits

object Preamble extends Conversions with Implicits {
    implicit def collAsScala(coll: DBCollection) = new {
        def asScala = new DBObjectCollection(coll)
    }

    implicit def queryToColl(q: Query) = new {
        def in[T, Self <: QueriedCollection[T, Self]](coll: QueriedCollection[T, Self]): Self = coll.applied(q)
    }

    implicit def wrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying
}