package com.osinka.mongodb.shape

import com.mongodb.{DBCollection, DBObject}
import wrapper._

class ShapedCollection[T](override val underlying: DBCollection, val shape: DBObjectShape[T])
        extends MongoCollection[T]
        with QueriedCollection[T, ShapedCollection[T]] {

    private lazy val shapeConstraints = DBO.fromMap(shape.constraints)
    private def embedShapeConstraints(q: DBObject) = DBO.merge(shapeConstraints, q)

    // -- MongoCollection[T]
    override val serializer: Serializer[T] = shape

    // -- QueriedCollection[T]
    override val query: Query = Query.empty

    override def applied(q: Query) = new ShapedCollection[T](underlying, shape) {
        override val query = q
    }

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(embedShapeConstraints(q))
    override def findOne(q: DBObject) = underlying.findOne(embedShapeConstraints(q))
    override def getCount(q: DBObject) = find(q).count

    override def stringPrefix: String = "ShapedCollection["+shape.getClass.getName+"]("+getName+")"
}