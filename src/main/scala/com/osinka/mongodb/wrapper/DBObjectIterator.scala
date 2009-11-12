package com.osinka.mongodb.wrapper

import com.mongodb.{DBObject, DBCursor}

private[mongodb] class DBObjectIterator(val cursor: DBCursor) extends Iterator[DBObject] {
    override def hasNext: Boolean = cursor.hasNext
    override def next: DBObject = cursor.next
}