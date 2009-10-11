package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}
import Helper._

case class Query(val query: DBObject, val skip: Option[Int], val limit: Option[Int]) {
    def slice_? = skip.isDefined || limit.isDefined

    def drop(n: Option[Int]) = Query(query, n, limit)

    def take(n: Option[Int]) = Query(query, skip, n)

    def drop(n: Int): Query = drop(Some(n))

    def take(n: Int): Query = take(Some(n))

    def *(q: Query): Query = ++(q.query) drop q.skip take q.limit

    def ++(q: DBObject): Query = Query(merge(query, q), skip, limit)
}

case object EmptyQuery extends Query(Query.Empty, None, None)

object Query {
    val Empty = new BasicDBObject

    def apply(): Query = apply(Empty)
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