package com.osinka.mongodb

import com.mongodb._
import scala.{Iterator => SIterator, Iterable => SIterable, Collection => SCollection}

class TypedDBOIterator[+T](val cursor: DBCursor) extends SIterator[T] {


    override def hasNext: Boolean = cursor.hasNext
    override def next: T = cursor.next.asInstanceOf[T]
}

trait TypedDBOIterable[+T] extends SIterable[T] with DBCollectionWrapper {
    override def elements: Iterator[T] = find

    def find: Iterator[T] = new TypedDBOIterator[T](underlying.find)

    def find(q: Query): Iterator[T] = new TypedDBOIterator[T](q.query.map{ underlying find _ } getOrElse underlying.find)

    def firstOption: Option[T] = underlying.findOne.asInstanceOf[T] match {
        case null => None
        case v => Some(v)
    }

    def headOption = firstOption

    // Beware: narrowing!
    def length: Int = longSize.toInt

    def longSize = underlying.getCount
}

class TypedDBOCollection[+T](override val underlying: DBCollection) extends SCollection[T] with TypedDBOIterable[T] {
    override def size: Int = length
    override def stringPrefix: String = "DBCollection[" + underlying.getObjectClass.getName + "]"
}
