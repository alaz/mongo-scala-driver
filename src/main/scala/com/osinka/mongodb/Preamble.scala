package com.osinka.mongodb

import com.mongodb.DBCollection

object Preamble {
    implicit def dbCollToWrapper(coll: DBCollection) = new OriginWrapper(coll)
    implicit def WrapperToDBO(coll: OriginProxy): DBCollection = coll.underlying
}
