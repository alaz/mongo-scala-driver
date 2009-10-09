package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
    }

//    implicit def shapeToQuery[T, S <: Shape[T]](shape: S) = new ShapeQuery[T, S](shape)

//    implicit def shapeToQuery[T, A <: Shape[T]](shape: A) = new {
//        def where(f: (A => QueryTerm[A])) = ShapeQuery[T,A](shape, f(shape), None, None)
//    }

    implicit def collWithQuery[T <: MongoObject](q: ShapeQuery[T]) = new {
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(q.query)
    }

//    def where[T, S <: Shape[T]](shape: S)(f : (S => QueryTerm[S])) = new ShapeQuery[T, S](shape) where f
}
