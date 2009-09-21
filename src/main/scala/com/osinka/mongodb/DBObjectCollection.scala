package com.osinka.mongodb

import com.mongodb._
import Helper._
import serializer.PlainDBOSerializer

class DBObjectCollection(override val underlying: DBCollection) extends MongoCollection[DBObject] with PlainDBOSerializer {
//    override val builder = PlainDBOBuilder

//    override def find(q: Query): Iterator[DBObject] = new DBObjectIterator(q.query.map{ underlying find _ } getOrElse underlying.find)

    override def firstOption: Option[DBObject] = tryo(underlying.findOne)

    override def sizeEstimate = underlying.getCount

    override def stringPrefix: String = "DBObjectCollection"

    override def <<(o: DBObject): Option[DBObject] = Some(underlying.insert(o))

    override def <<?(obj: DBObject): Option[DBObject] = {
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

    override def +=(obj: DBObject): Option[DBObject] = Some(underlying.save(obj))

    override def -=(obj: DBObject) { underlying.remove(obj) }
}