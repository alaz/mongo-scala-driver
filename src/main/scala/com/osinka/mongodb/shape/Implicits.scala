package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
    }

    implicit def collWithQuery[T](q: Queriable[T]#ShapeQuery) = new {
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(q.query)
    }

    implicit def collWithQuery[T <: MongoObject](qt: QueryTerm[T]) = new {
        def in(coll: ShapedCollection[T]): ShapedCollection[T] = {
            val shapeQuery = coll.shape where qt
            coll.applied(shapeQuery.query)
        }
    }
}