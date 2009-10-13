package com.osinka.mongodb.shape

import com.mongodb._
import Helper._

class ShapedCollection[T <: MongoObject](override val underlying: DBCollection, override val element: MongoObjectShape[T])
        extends MongoCollection[T] with ShapedSerializer[T] with QueriedCollection[T, ShapedCollection[T]] {

    // -- QueriedCollection[T]
    type Self = ShapedCollection[T]

    override val query: Query = Query.empty

    override def applied(q: Query) = new ShapedCollection[T](underlying, element) {
        override val query = q
    }

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(q, element.shape)
    override def findOne(q: DBObject) = underlying.findOne(q, element.shape)
    override def getCount(q: DBObject) = underlying.getCount(q, element.shape)

    override def stringPrefix: String = "ShapedCollection["+element.getClass.getName+"]("+getName+")"
}