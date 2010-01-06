package com.osinka.mongodb.shape

import com.mongodb.{DBCollection, DBObject}
import wrapper._

class ShapedCollection[T](override val underlying: DBCollection, val shape: ObjectShape[T])
        extends MongoCollection[T]
        with QueriedCollection[T, ShapedCollection[T]] {

    private lazy val shapeConstraints = DBO.fromMap(shape.constraints)
    private def embedShapeConstraints(q: DBObject) = DBO.merge(shapeConstraints, q)

    def update(filters: QueryTerm[T], op: ModifyOp[T], multi: Boolean): Boolean = update(filters.query, op.m, multi)
    
    def update(filters: QueryTerm[T], op: ModifyOp[T]): Boolean = update(filters, op, false)

    def updateAll(filters: QueryTerm[T], op: ModifyOp[T]): Boolean = update(filters, op, true)

    // -- MongoCollection[T]
    override val serializer: Serializer[T] = shape

    // -- QueriedCollection[T]
    override val query: Query = Query.empty
    override def applied(q: Query): ShapedCollection[T] = new ShapedCollection[T](underlying, shape) {
        override val query = q
    }

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(embedShapeConstraints(q))
    override def findOne(q: DBObject) = underlying.findOne(embedShapeConstraints(q))
    override def getCount(q: DBObject) = find(q).count
    override def update(q: DBObject, op: DBObject, multi: Boolean) = super.update(embedShapeConstraints(q), op, multi)

    override def stringPrefix: String = "ShapedCollection["+shape.getClass.getName+"]"
}