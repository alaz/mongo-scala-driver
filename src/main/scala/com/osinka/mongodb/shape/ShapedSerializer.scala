package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    override val in = new PartialFunction[T, DBObject] {
        def isDefinedAt(obj: T) = true

        def apply(obj: T) = {
            val dbo = BasicDBObjectBuilder.start.get
            element(dbo) = obj
            dbo
        }
    }

    override val out = new PartialFunction[DBObject, T] {
        def isDefinedAt(dbo: DBObject) = true

        def apply(dbo: DBObject) = element(dbo)
    }

    override def mirror(x: T)(dbo: DBObject) = {
        for {f <- element.* if f.internal_?
             val v = dbo.get(f.name) } f.setter(x, dbo.get(f.name))
        x
    }
}
