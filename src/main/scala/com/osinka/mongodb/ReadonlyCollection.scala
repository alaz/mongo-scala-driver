package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._
import Helper._

trait ReadonlyCollection[+T] extends Collection[T] with DBCollectionWrapper {
    protected val out: PartialFunction[DBObject, T]

    protected def find(q: Query): Iterator[T] =
        new DBObjectIterator(underlying find q.query).filter{out.isDefinedAt}.map{out}

//    val builder: Builder[T]

//    def mutable: MutableCollection[T] = builder.mutable(this)

    override def elements: Iterator[T] = find

    def find: Iterator[T] = find(EmptyQuery)

    def firstOption: Option[T] = find.take(1).collect.firstOption

    def headOption = firstOption

    // Beware: narrowing!
    override def size: Int = length
    def length: Int = sizeEstimate.toInt

    // Rough size estimates the collection size: it does not take object shape into account
    def sizeEstimate = underlying.getCount

    override def stringPrefix: String = "mongodb.ImmutableCollection"
}

class DBObjectReadonlyCollection(override val underlying: DBCollection) extends ReadonlyCollection[DBObject] {
    override val out: PartialFunction[DBObject, DBObject] = { case x => x }
//    override val builder = PlainDBOBuilder

//    override def find(q: Query): Iterator[DBObject] = new DBObjectIterator(q.query.map{ underlying find _ } getOrElse underlying.find)

    override def firstOption: Option[DBObject] = tryo(underlying.findOne)

    override def sizeEstimate = underlying.getCount

    override def stringPrefix: String = "DBObjectCollection"
}