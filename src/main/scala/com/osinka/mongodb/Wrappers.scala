package com.osinka.mongodb

import com.mongodb.{DBCollection, DBObject, DBCursor}

trait DBCollectionWrapper {
    val underlying: DBCollection

    protected def find(dbo: DBObject) = underlying find dbo
    protected def findOne(dbo: DBObject) = underlying findOne dbo
    protected def getCount(dbo: DBObject) = underlying getCount dbo

    def getName = underlying.getName

    def getFullName = underlying.getFullName

    def drop: Unit = underlying.drop

    override def equals(obj: Any) = obj match {
        case other: DBCollectionWrapper => underlying.equals(other.underlying)
        case _ => false
    }
}

private[mongodb] class DBObjectIterator(val cursor: DBCursor) extends Iterator[DBObject] {
    override def hasNext: Boolean = cursor.hasNext
    override def next: DBObject = cursor.next
}