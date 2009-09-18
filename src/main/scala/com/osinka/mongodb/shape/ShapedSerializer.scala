package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    protected val in: PartialFunction[T, DBObject] = {
        case obj: T =>
            val dbo = BasicDBObjectBuilder.start.get
            element(dbo) = obj
            dbo
    }

    protected val out: PartialFunction[DBObject, T] = {
        case dbo: DBObject => element(dbo)
    }
}
