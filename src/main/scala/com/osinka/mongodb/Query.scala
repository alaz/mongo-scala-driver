package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}
import Helper._

case class Query(final val query: DBObject, val skip: Option[Int], val limit: Option[Int]) {
    def slice_? = skip.isDefined || limit.isDefined

    def drop(n: Option[Int]) = Query(query, n, limit)

    def take(n: Option[Int]) = Query(query, skip, n)

    def drop(n: Int): Query = drop(Some(n))

    def take(n: Int): Query = take(Some(n))

    def *(q: Query): Query = ++(q.query) drop q.skip take q.limit

    def ++(q: DBObject): Query = Query(merge(query, q), skip, limit)
}

object Query {
    final val empty = Query(emptyDBO, None, None)

    def apply(): Query = empty
    def apply(q: DBObject) = new Query(q, None, None)
}

trait QueriedCollection[T, Self <: QueriedCollection[T, Self]] extends MongoCollection[T] {
    def query: Query

    def applied(q: Query): Self

    // -- MongoCollection[T]
    override def find = find(query)
    override def firstOption = findOne(query)
    override def sizeEstimate = getCount(query)
}