package com.osinka.mongodb.shape

import com.mongodb.DBObject
import serializer.Conversions

case class QueryTerm[+Host](val m: Map[String, Any]) {
    def this() {
        this(Map.empty[String,Any])
    }

    def this(tuple: (String, Any)) {
        this(Map(tuple))
    }

    def &&[B >: Host](q: QueryTerm[B]) = new QueryTerm[Host](m ++ q.m)
}

case class ShapeQuery[T <: MongoObject](val shape: Shape[T], val q: QueryTerm[T], val skip: Option[Int], val limit: Option[Int]) extends Conversions {
    def this(h: Shape[T]) {
        this(h, new QueryTerm[T], None, None)
    }

    def where(query: QueryTerm[T]) = new ShapeQuery[T](shape, q && query, skip, limit)

    def drop(n: Option[Int]): ShapeQuery[T] = new ShapeQuery(shape, q, n, limit)

    def take(n: Option[Int]): ShapeQuery[T] = new ShapeQuery(shape, q, skip, n)

    def drop(n: Int): ShapeQuery[T] = drop(Some(n))

    def take(n: Int): ShapeQuery[T] = take(Some(n))

    def query = Query(createDBObject(q.m), skip, limit)
}