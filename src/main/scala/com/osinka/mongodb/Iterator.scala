package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._

/*
private[mongodb] class TypedIterator[+T](val cursor: DBCursor, val serializer: PartialFunction[DBObject, T]) extends BufferedIterator[T] {
    private var headObj: Option[T] = None

    override def hasNext: Boolean =
        if (cursor.hasNext) {
            headObj = serializer.out(cursor.next)
            headObj.isDefined || hasNext
        } else
            false

    override def next: T =  {
        headObj = serializer.out(cursor.next)
        headObj getOrElse next
    }

    override def headOpt = headObj
    override def head: T = headObj getOrElse next
}
*/

private[mongodb] class DBObjectIterator(val cursor: DBCursor) extends Iterator[DBObject] {
    override def hasNext: Boolean = cursor.hasNext
    override def next: DBObject = cursor.next
}