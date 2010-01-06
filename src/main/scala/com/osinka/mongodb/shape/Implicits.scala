package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T](element: ObjectShape[T]) = element.collection(coll)
    }

    implicit def collWithQuery[T](q: Queriable[T]#ShapeQuery) = new {
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(q.query)
    }

    implicit def collWithQuery[T](qt: QueryTerm[T]) = new {
        def in(coll: ShapedCollection[T]): ShapedCollection[T] = {
            val shapeQuery = coll.shape where qt
            coll.applied(shapeQuery.query)
        }
    }

    implicit def queryTofilters[T](q: Queriable[T]#ShapeQuery): QueryTerm[T] = q.filters

    implicit def shapeTofilters[T](q: ObjectShape[T]): QueryTerm[T] = QueryTerm[T]()
}