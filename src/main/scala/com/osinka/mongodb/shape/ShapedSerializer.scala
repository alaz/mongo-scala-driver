package com.osinka.mongodb.shape

import com.mongodb._
import com.osinka.mongodb.serializer.Serializer

trait ShapedSerializer[T <: MongoObject] extends Serializer[T] {
    def element: Shape[T]

    protected val in: PartialFunction[T, DBObject] = {
        case obj: T =>
            BasicDBObjectBuilder.start.get
    }

    protected val out: PartialFunction[DBObject, T] = {
        case dbo: DBObject =>
            val x = element.factory(dbo)
            // for { val f <- element.* }
                // setter
            x
    }
}
