package com.osinka.mongodb.shape

import com.mongodb._
import Helper._

class ShapedCollection[T <: MongoObject](override val underlying: DBCollection,
                                         override val element: Shape[T])
        extends MongoCollection[T] with ShapedSerializer[T] {

    override def find(q: Query) = super.find(q ++ element.shape)

    override def firstOption: Option[T] = tryo(underlying.findOne(Query.Empty, element.shape)).filter{out.isDefinedAt}.map{out}

    override def sizeEstimate = underlying.getCount(Query.Empty, element.shape)
}