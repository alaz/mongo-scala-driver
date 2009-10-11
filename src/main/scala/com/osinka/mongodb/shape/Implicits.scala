package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
    }

    implicit def collWithQuery[T](q: ShapeQuery[T]) = new {
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(q.query)
    }

    implicit def collWithQuery[T](qt: QueryTerm[T]) = new {
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied((ShapeQuery[T] where qt).query)
    }
}
