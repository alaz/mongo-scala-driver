package com.osinka.mongodb

import com.mongodb._
import Helper._
import serializer.PlainDBOSerializer

class DBObjectCollection(override val underlying: DBCollection)
        extends MongoCollection[DBObject] with QueriedCollection[DBObject, DBObjectCollection] with PlainDBOSerializer {

    // -- QueriedCollection[T]
    override val query: Query = EmptyQuery
    
    override def applied(q: Query) = new DBObjectCollection(underlying) {
        override val query = q
    }

    // -- MongoCollection
    override def stringPrefix: String = "DBObjectCollection"

    override def <<(o: DBObject): DBObject = underlying.insert(o)

    override def <<?(obj: DBObject): Option[DBObject] = {
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

    override def +=(obj: DBObject): DBObject = underlying.save(obj)

    override def -=(obj: DBObject) { underlying.remove(obj) }
}