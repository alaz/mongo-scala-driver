package com.osinka.mongodb.shape

import com.mongodb.{DBCollection, DBObject}
import wrapper._

class ShapedCollection[T <: MongoObject](override val underlying: DBCollection, override val element: MongoObjectShape[T])
        extends MongoCollection[T] with ShapedSerializer[T] with QueriedCollection[T, ShapedCollection[T]] {

    // -- MongoCollection[T]
    override val serializer: Serializer[T] = element

    // -- QueriedCollection[T]
    override val query: Query = Query.empty

    override def applied(q: Query) = new ShapedCollection[T](underlying, element) {
        override val query = q
    }

    private lazy val shapeQuery = DBO.fromMap(element.shape)
    private def embedShapeConstraints(q: DBObject) = DBO.merge(shapeQuery, q)

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(embedShapeConstraints(q))
    override def findOne(q: DBObject) = underlying.findOne(embedShapeConstraints(q))
    override def getCount(q: DBObject) = find(q).count

    override def stringPrefix: String = "ShapedCollection["+element.getClass.getName+"]("+getName+")"
}