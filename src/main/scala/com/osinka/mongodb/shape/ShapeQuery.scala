package com.osinka.mongodb.shape

import Preamble._

trait Queriable[T] { self: ObjectShape[T] =>
    type SortableFieldType = FieldIn with FieldCond[T, _]

    def where(query: QueryTerm[T]) = ShapeQuery() where query
    def drop(n: Int) = ShapeQuery() drop n
    def take(n: Int) = ShapeQuery() take n
    def sortBy(sorting: (SortableFieldType, SortOrder)*) = ShapeQuery().sortBy(sorting:_*)

    case class ShapeQuery(val filters: QueryTerm[T], val sortBy: List[(SortableFieldType, SortOrder)], private val q: Query) {
        def where(filter: QueryTerm[T]) = ShapeQuery(filters and filter, sortBy, q)

        def drop(n: Int): ShapeQuery = drop(Some(n))
        def drop(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q drop n)

        def take(n: Int): ShapeQuery = take(Some(n))
        def take(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q take n)

        def noSort = ShapeQuery(filters, sortBy, q sort None)
        def sortBy(s: (SortableFieldType, SortOrder)*): ShapeQuery = ShapeQuery(filters, s.toList ::: sortBy, q)

        def query: Query = {
            val s = (Map.empty[String, Int] /: sortBy) { (m, x) =>
                m + (dotNotation(x._1.mongoFieldPath) -> x._2.mongoOrder)
            }
            q ++ filters.m sort s
        }
    }

    object ShapeQuery {
        def apply() = new ShapeQuery(QueryTerm[T], Nil, Query())
        def apply(qt: QueryTerm[T]) = new ShapeQuery(qt, Nil, Query())
    }

    // TODO: move QueryTerm here. Subclassing?
}

sealed case class QueryTerm[+T](val m: Map[String, Any]) {
    def and[B >: T](q: QueryTerm[B]) = new QueryTerm[T](m ++ q.m)
}

object QueryTerm {
    def apply[T]() = new QueryTerm[T](Map.empty[String, Any])
    def apply[T](tuple: (String, Any)) = new QueryTerm[T](Map(tuple))
}