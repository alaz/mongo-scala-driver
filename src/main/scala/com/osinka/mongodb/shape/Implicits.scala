package com.osinka.mongodb.shape

import com.mongodb.DBCollection

trait Implicits {
    implicit def collOfShape(coll: DBCollection) = new {
        def of[T <: MongoObject](element: Shape[T]) = new ShapedCollection[T](coll, element)
    }

    implicit def shapeToQuery[T <: MongoObject, S <: Shape[T]](shape: S) = new ShapeQuery[S](shape)

    implicit def collWithQuery[S](q: ShapeQuery[S]) = new {
        def in[T, Self <: QueriedCollection[T, Self]](coll: QueriedCollection[T, Self]): Self = coll.applied(q.query)
    }

    implicit def fieldToQuery[Host, A](f: Field[Host, A, _]) = FieldQuery[Host, A](f)
}
