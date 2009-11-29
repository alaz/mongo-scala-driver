package com.osinka.mongodb

import com.mongodb._
import wrapper._

trait MongoCollection[T] extends Iterable[T] with DBCollectionWrapper {
    def serializer: Serializer[T]

    protected def cursor(q: Query) = {
        val cursor = find(q.query)
        for {n <- q.skip } cursor.skip(n)
        for {n <- q.limit} cursor.limit(n)
        for {sort <- q.sorting} cursor.sort(sort)
        // TODO: snapshot mode
        cursor
    }

    protected def find(q: Query): Iterator[T] =
        new DBObjectIterator(cursor(q)).flatMap{serializer.out(_).toList.iterator}

    protected def findOne(q: Query): Option[T] =
        if (q.slice_?) find(q take 1).toSeq.headOption
        else Option(findOne(q.query)).flatMap{serializer.out}

    protected def getCount(q: Query): Long = {
        def lim(n: Int) = q.limit map{_ min n} getOrElse n
        def skp(n: Int) = q.skip map{x => (n - x) max 0} getOrElse n

        if (q.slice_?) lim(skp(cursor(q).count))
        else getCount(q.query)
    }

    def find: Iterator[T] = find(Query.empty)

    // Rough size estimates the collection size: it does not take object shape into account
    def sizeEstimate = getCount(Query.empty)

    def <<(x: T): T = serializer.mirror(x)(underlying insert serializer.in(x))

    def <<?(x: T): Option[T] = {
        val r = underlying insert serializer.in(x)
        underlying.getBase.getLastError get "err" match {
            case null => Some( serializer.mirror(x)(r) )
            case msg: String => None
        }
    }

    def +=(x: T): T = serializer.mirror(x)( underlying save serializer.in(x) )

    def -=(x: T) { underlying remove serializer.in(x) }

    // -- Collection[T]
    override def iterator: Iterator[T] = find

    override def headOption = findOne(Query.empty)

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
    override def toString = stringPrefix+"("+getName+"):"+size
}