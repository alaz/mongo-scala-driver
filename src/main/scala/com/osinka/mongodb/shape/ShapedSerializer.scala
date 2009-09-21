package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    override def in(obj: T) = {
        val dbo = BasicDBObjectBuilder.start.get
        element(dbo) = obj
        dbo
    }

    override def out(dbo: DBObject) = element(dbo)

    override def mirror(x: T)(dbo: DBObject) = {
        for {f <- element.* if f.mongo_?
             val v = dbo.get(f.name) } f.setter(x, dbo.get(f.name))
        x
    }
}
