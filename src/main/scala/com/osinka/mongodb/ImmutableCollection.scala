package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._

trait ImmutableCollection[+T] extends Collection[T] with DBCollectionWrapper {
    val out: PartialFunction[DBObject, T]
//    val builder: Builder[T]

//    def mutable: MutableCollection[T] = builder.mutable(this)

    override def elements: Iterator[T] = find

    def find: Iterator[T] = find(EmptyQuery)

    def find(q: Query): Iterator[T] =
        new DBObjectIterator(q.query.map{ underlying find _ } getOrElse underlying.find).filter{out.isDefinedAt}.map{out.apply}

    def firstOption: Option[T] = underlying.findOne match {
        case null => None
        case v if out.isDefinedAt(v) => Some(out(v))
        case _ => None
    }

    def headOption = firstOption

    // Beware: narrowing!
    override def size: Int = length
    def length: Int = roughSizeEstimate.toInt

    // Rough size estimates the collection size
    def roughSizeEstimate = underlying.getCount
    // TODO: Accurate size estimates the collection size taking type fields into account

    override def stringPrefix: String = "mongodb.ImmutableCollection"
}

class DBObjectImmutableCollection(override val underlying: DBCollection) extends ImmutableCollection[DBObject] with DBCollectionWrapper {
    override val out: PartialFunction[DBObject, DBObject] = { case x => x }
//    override val builder = PlainDBOBuilder

//    override def find(q: Query): Iterator[DBObject] = new DBObjectIterator(q.query.map{ underlying find _ } getOrElse underlying.find)

    override def stringPrefix: String = "DBObjectCollection"
}