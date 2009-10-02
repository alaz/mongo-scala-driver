package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer
import Helper._

trait Transformer[Type, Rep] {
    def extract(x: Rep): Option[Type] // a kind of "unapply"
    def pack(v: Type): Rep // a kind of "apply"
}

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    override def in(obj: T) = element.pack(obj)

    override def out(dbo: DBObject) = element.extract(dbo)

    override def mirror(x: T)(dbo: DBObject) = {
        for {val f <- element.* if f.mongo_? && f.isInstanceOf[HostUpdate[_,_]]
             val fieldDbo <- tryo(dbo.get(f.name))}
            f.asInstanceOf[HostUpdate[T,_]].updateUntyped(x, fieldDbo)
        x
    }
}
