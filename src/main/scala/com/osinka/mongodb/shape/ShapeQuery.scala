package com.osinka.mongodb.shape

import com.mongodb.DBObject
import serializer.Conversions

trait Queriable[T] {
    def where(query: QueryTerm[T]) = ShapeQuery[T] where query
    def drop(n: Int) = ShapeQuery[T] drop n
    def take(n: Int) = ShapeQuery[T] take n
    def sortBy(sorting: (FieldCond[T, _, _], SortOrder)*) = ShapeQuery[T].sortBy(sorting:_*)
}

case class QueryTerm[+T](val m: Map[String, Any]) {
    def &&[B >: T](q: QueryTerm[B]) = new QueryTerm[T](m ++ q.m)
}

object QueryTerm {
    def apply[T]() = new QueryTerm[T](Map.empty[String, Any])
    def apply[T](tuple: (String, Any)) = new QueryTerm[T](Map(tuple))
}

case class ShapeQuery[T](val q: QueryTerm[T],
                         val skip: Option[Int],
                         val limit: Option[Int],
                         val sorting: List[(FieldCond[T,_,_], SortOrder)]) extends Conversions {
    
    def where(query: QueryTerm[T]) = ShapeQuery[T](q && query, skip, limit, sorting)

    def drop(n: Option[Int]): ShapeQuery[T] = ShapeQuery(q, n, limit, sorting)

    def take(n: Option[Int]): ShapeQuery[T] = ShapeQuery(q, skip, n, sorting)

    def drop(n: Int): ShapeQuery[T] = drop(Some(n))

    def take(n: Int): ShapeQuery[T] = take(Some(n))

    def sortBy(s: (FieldCond[T, _, _], SortOrder)*) = ShapeQuery(q, skip, limit, s.toList ::: sorting)

    def query = {
        val s = (Map.empty[String, Int] /: sorting) {(m, x) => m + (x._1.mongoFieldName -> x._2.mongoOrder)}
        Query(createDBObject(q.m), skip, limit, if (s.isEmpty) None else Some(createDBObject(s)) )
    }
}

object ShapeQuery {
    def apply[T]() = new ShapeQuery(QueryTerm[T], None, None, Nil)
    def apply[T](qt: QueryTerm[T]) = new ShapeQuery(qt, None, None, Nil)
}