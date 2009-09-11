package com.osinka.mongodb

import com.mongodb._

trait DBObjectMutableStore extends DBCollectionWrapper {
    def insert(o: DBObject): DBObject = underlying.insert(o)

    def insert_?(obj: DBObject): Option[DBObject] = {
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    def save(obj: DBObject): DBObject = underlying.save(obj)

    def remove(obj: DBObject) { underlying.remove(obj) }

    def update {}
}

private[mongodb] class DBObjectIterator(val cursor: DBCursor) extends Iterator[DBObject] {
    override def hasNext: Boolean = cursor.hasNext
    override def next: DBObject = cursor.next
}

trait DBObjectIterable extends Iterable[DBObject] with DBCollectionWrapper {
    override def elements: Iterator[DBObject] = find

    def find: Iterator[DBObject] = new DBObjectIterator(underlying.find)

    def find(q: Query): Iterator[DBObject] = new DBObjectIterator(q.query.map{ underlying find _ } getOrElse underlying.find)

    def firstOption: Option[DBObject] = underlying.findOne match {
        case null => None
        case v => Some(v)
    }

    def headOption = firstOption

    // Beware: narrowing!
    def length: Int = longSize.toInt

    def longSize = underlying.getCount
}

class DBObjectCollection(override val underlying: DBCollection) extends Collection[DBObject] with DBObjectIterable with DBObjectMutableStore {
    override def size: Int = length
    override def stringPrefix: String = "DBObjectCollection"
}