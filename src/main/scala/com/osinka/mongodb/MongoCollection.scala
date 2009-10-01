package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._
import Helper._

trait MongoCollection[T] extends Collection[T] with Serializer[T] with DBCollectionWrapper {
    protected def find(q: Query): Iterator[T] = new DBObjectIterator(underlying find q.query).flatMap{out(_).toList.elements}

    protected def findOne(q: Query): Option[T] = tryo(underlying findOne q.query).flatMap{out}

    protected def getCount(q: Query) = underlying getCount q.query

//    val builder: Builder[T]

//    def mutable: MutableCollection[T] = builder.mutable(this)

    override def elements: Iterator[T] = find

    def find: Iterator[T] = find(EmptyQuery)

    def firstOption: Option[T] = findOne(EmptyQuery)

    def headOption = firstOption

    // Beware: narrowing!
    override def size: Int = length
    def length: Int = sizeEstimate.toInt

    // Rough size estimates the collection size: it does not take object shape into account
    def sizeEstimate = getCount(EmptyQuery)

    override def stringPrefix: String = "MongoCollection"

    // TODO: return T

    def <<(x: T): T = mirror(x)(underlying insert in(x))

    def <<?(x: T): Option[T] = {
        val r = underlying insert in(x)
        underlying.getBase.getLastError get "err" match {
            case null => Some( mirror(x)(r) )
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    def +=(x: T): T = mirror(x)( underlying save in(x) )

    def -=(x: T) { underlying remove in(x) }
}