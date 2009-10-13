package com.osinka.mongodb

import com.mongodb._
import com.osinka.mongodb.serializer._
import Helper._

trait MongoCollection[T]
        extends Collection[T] with Serializer[T] with DBCollectionWrapper {

    protected def cursor(q: Query) = {
        val cursor = find(q.query)
        for {val n <- q.skip } cursor.skip(n)
        for {val n <- q.limit} cursor.limit(n)
        // TODO: snapshot mode
        // TODO: sort
        cursor
    }

    protected def find(q: Query): Iterator[T] =
        new DBObjectIterator(cursor(q)).flatMap{out(_).toList.elements}

    protected def findOne(q: Query): Option[T] =
        if (q.slice_?) find(q take 1).collect.firstOption
        else tryo(findOne(q.query)).flatMap{out}

    protected def getCount(q: Query): Long = {
        def lim(n: Int) = q.limit map{_ min n} getOrElse n
        def skp(n: Int) = q.skip map{x => (n - x) max 0} getOrElse n

        if (q.slice_?) lim(skp(cursor(q).count))
        else getCount(q.query)
    }

    def find: Iterator[T] = find(Query.empty)

    // Rough size estimates the collection size: it does not take object shape into account
    def sizeEstimate = getCount(Query.empty)

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

    // -- Collection[T]
    override def elements: Iterator[T] = find

    def firstOption: Option[T] = findOne(Query.empty)

    def headOption = firstOption

    /**
     * Size of the collection
     *
     * NOTE: Original MongoDB cursor reports collection's size regardless
     * of skip and limit modificators. This implementation takes these into
     * account: you may expect to get accurate collection length when
     * you called drop and take on Query object.
     *
     * Beware: narrowing as Long value of getCount is cast to Int
     */
    override def size: Int = length
    def length: Int = sizeEstimate.toInt

    override def stringPrefix: String = "MongoCollection"
}