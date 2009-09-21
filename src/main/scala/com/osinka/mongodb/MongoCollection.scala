package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._
import Helper._

trait MongoCollection[T] extends Collection[T] with Serializer[T] with DBCollectionWrapper {
    protected def find(q: Query): Iterator[T] =
        new DBObjectIterator(underlying find q.query).filter{out.isDefinedAt}.map{out}

    protected def getCount(q: Query) = underlying getCount q.query

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
    def sizeEstimate = getCount(EmptyQuery)

    override def stringPrefix: String = "MongoCollection"

    // TODO: return T

    def <<(x: T): Option[T] = pfToOption(in andThen underlying.insert andThen mirror(x))(x)

    def <<?(x: T): Option[T] = pfToOption(in)(x) flatMap { obj =>
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(mirror(x)(r))
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    def +=(x: T): Option[T] = pfToOption(in andThen underlying.save andThen mirror(x))(x)

    def -=(x: T) { (in andThen underlying.remove)(x) }
}