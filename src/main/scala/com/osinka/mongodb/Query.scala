package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}
import wrapper._
import Preamble._

case class Query(final val query: DBObject,
                 val skip: Option[Int],
                 val limit: Option[Int],
                 val sorting: Option[DBObject]) {

    def slice_? = skip.isDefined || limit.isDefined || sorting.isDefined

    def drop(n: Option[Int]) = Query(query, n, limit, sorting)

    def take(n: Option[Int]) = Query(query, skip, n, sorting)

    def drop(n: Int): Query = drop(Some(n))

    def take(n: Int): Query = take(Some(n))

    def sort(s: Option[DBObject]): Query = Query(query, skip, limit, s)

    def sort(s: DBObject): Query = sort(Some(s))

    def *(q: Query): Query = ++(q.query) drop q.skip take q.limit sort q.sorting

    def ++(q: DBObject): Query = Query(DBO.merge(query, q), skip, limit, sorting)
}

object Query {
    final val empty = Query(DBO.empty, None, None, None)

    def apply(): Query = empty
    def apply(q: DBObject) = new Query(q, None, None, None)
}

trait QueriedCollection[T, Self <: QueriedCollection[T, Self]] extends MongoCollection[T] {
    def query: Query

    def applied(q: Query): Self

    // -- MongoCollection[T]
    override def find = find(query)
    override def headOption = findOne(query)
    override def sizeEstimate = getCount(query)
}