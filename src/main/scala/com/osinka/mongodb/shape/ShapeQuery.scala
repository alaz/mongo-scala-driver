package com.osinka.mongodb.shape

import com.mongodb.DBObject
import serializer.Conversions

case class QueryTerm(val m: Map[String, Any]) {
    def this() {
        this(Map.empty[String,Any])
    }

    def this(tuple: (String, Any)) {
        this(Map(tuple))
    }

    def &&(q: QueryTerm) = new QueryTerm(m ++ q.m)
}

//trait ShapeQueryOps[T <: MongoObject] { self: Shape[T] =>
//    def where[B >: Shape[T]](f: (B => QueryTerm)): ShapeQuery[B] = new ShapeQuery(self) where f
//
//    def drop(n: Int) = new ShapeQuery(self) drop n
//
//    def take(n: Int) = new ShapeQuery(self) take n
//}

case class ShapeQuery[T, +A <: Shape[T]](val shape: A, val q: QueryTerm, val skip: Option[Int], val limit: Option[Int]) extends Conversions {
    def this(h: A) {
        this(h, new QueryTerm, None, None)
    }

    def where[B >: A <: Shape[T]](f: (B => QueryTerm)): ShapeQuery[T, B] = new ShapeQuery(shape, q && f(shape), skip, limit)

    def drop(n: Option[Int]): ShapeQuery[T, A] = new ShapeQuery(shape, q, n, limit)

    def take(n: Option[Int]): ShapeQuery[T, A] = new ShapeQuery(shape, q, skip, n)

    def drop(n: Int): ShapeQuery[T, A] = drop(Some(n))

    def take(n: Int): ShapeQuery[T, A] = take(Some(n))

    def query = Query(createDBObject(q.m), skip, limit)
}