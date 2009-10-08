package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
    }

//    implicit def shapeToQuery[T, S <: Shape[T]](shape: S) = new ShapeQuery[T, S](shape)

//    implicit def shapeToQuery[S](shape: S) = new {
//        def where(f: (S => QueryTerm[S])) = new ShapeQuery(shape) where f
//    }

//    implicit def collWithQuery[T, S <: Shape[T]](q: ShapeQuery[T, S]) = new {
//        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(q.query)
//    }
}
