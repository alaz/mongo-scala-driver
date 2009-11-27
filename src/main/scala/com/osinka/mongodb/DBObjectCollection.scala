package com.osinka.mongodb

import com.mongodb.{DBCollection, DBObject}

class DBObjectCollection(override val underlying: DBCollection)
        extends MongoCollection[DBObject]
        with QueriedCollection[DBObject, DBObjectCollection] {

    override val serializer: Serializer[DBObject] = PlainDBOSerializer

    // -- QueriedCollection[T]
    override val query: Query = Query.empty
    override def applied(q: Query): DBObjectCollection = new DBObjectCollection(underlying) {
        override val query = q
    }

    // -- MongoCollection
    override def stringPrefix: String = "DBObjectCollection("+getName+")"

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

object PlainDBOSerializer extends Serializer[DBObject] {
    override def in(obj: DBObject) = obj
    override def out(dbo: DBObject) = Some(dbo)
    override def mirror(x: DBObject)(dbo: DBObject) = dbo
}