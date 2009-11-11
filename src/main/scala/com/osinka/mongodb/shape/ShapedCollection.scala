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

    private lazy val shapeQuery = Preamble.createDBObject(element.shape)
    private def embedShapeConstraints(q: DBObject) = merge(shapeQuery, q)

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(embedShapeConstraints(q))
    override def findOne(q: DBObject) = underlying.findOne(embedShapeConstraints(q))
    override def getCount(q: DBObject) = find(q).count

    override def stringPrefix: String = "ShapedCollection["+element.getClass.getName+"]("+getName+")"
}