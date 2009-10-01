package com.osinka.mongodb.shape

import com.mongodb._
import Helper._

class ShapedCollection[T <: MongoObject](override val underlying: DBCollection, override val element: Shape[T])
        extends MongoCollection[T] with ShapedSerializer[T] {

    override def find(q: Query) = super.find(q ++ element.shape)
    override def findOne(q: Query) = tryo( underlying.findOne(q.query, element.shape) ).flatMap{out}

    override def sizeEstimate = underlying.getCount(Query.Empty, element.shape)

    override def stringPrefix: String = "ShapedCollection[" + element.clazz + "]"
}