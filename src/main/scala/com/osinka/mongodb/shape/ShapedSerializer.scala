package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer
import Helper._

trait DBObjectStored[T] {
    def extract(x: DBObject): Option[T] // a kind of "unapply"
    def pack(v: T): DBObject // a kind of "apply"
}

/**
 * ShapeFunctionObject will provide shape with convinience syntactic sugar
 * for converting object to DBObject and extractor for opposite
 *
 * E.g.
 * val dbo = UserShape(u)
 * dbo match {
 *    case UserShape(u) =>
 * }
 *
 * The same applies to field shapes
 */
trait ShapeFunctionObject[T] { self: DBObjectStored[T] =>
    def apply(x: T): DBObject = pack(x)
    def unapply(dbo: DBObject): Option[T] = extract(dbo)
}

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    override def in(obj: T) = element.pack(obj)

    override def out(dbo: DBObject) = element.extract(dbo)

    override def mirror(x: T)(dbo: DBObject) = {
        for {val f <- element.* if f.mongo_? && f.isInstanceOf[HostUpdate[_,_]]
             val v <- tryo(dbo.get(f.name))}
            f.asInstanceOf[HostUpdate[T,_]].update(x, v)
        x
    }
}
