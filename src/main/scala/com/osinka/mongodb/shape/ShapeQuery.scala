package com.osinka.mongodb.shape

import com.mongodb.DBObject
import serializer.Conversions

trait FieldOps {
    def op(op: String)(name: String, x: Any) = name -> Map(op -> x)

    def eq(name: String, x: Any) = name -> x
    lazy val ne = op("$ne") _
    lazy val lt = op("$lt") _
    lazy val le = op("$lte") _
    lazy val gt = op("$gt") _
    lazy val ge = op("$gte") _
}

case class FieldQuery[Host, A](val f: Field[Host, A, _]) extends FieldOps {
    def <(x: A) = new QueryTerm + lt(f.name, x)
    def <=(x: A) = new QueryTerm + le(f.name, x)
    def >(x: A) = new QueryTerm + gt(f.name, x)
    def >=(x: A) = new QueryTerm + ge(f.name, x)
    def ==(x: A) = new QueryTerm + eq(f.name, x)
    def !=(x: A) = new QueryTerm + ne(f.name, x)
}

case class QueryTerm[+A](val m: Map[String, Any]) {
    def this() {
        this(Map.empty[String,Any])
    }

    def this(tuple: (String, Any)) {
        this(Map(tuple))
    }

    def +(tuple: (String, Any)) = new QueryTerm[A](m + tuple)

    def &&[B >: A](q: QueryTerm[B]) = new QueryTerm[B](m ++ q.m)
}

case class ShapeQuery[+A](val host: A, val q: QueryTerm[A], val skip: Option[Int], val limit: Option[Int]) extends Conversions {
    def this(h: A) {
        this(h, new QueryTerm[A], None, None)
    }

    def where[B >: A](f: (B => QueryTerm[B])): ShapeQuery[B] = new ShapeQuery(host, q && f(host), skip, limit)

    def drop(n: Option[Int]): ShapeQuery[A] = new ShapeQuery(host, q, n, limit)

    def take(n: Option[Int]): ShapeQuery[A] = new ShapeQuery(host, q, skip, n)

    def drop(n: Int): ShapeQuery[A] = drop(Some(n))

    def take(n: Int): ShapeQuery[A] = take(Some(n))

    def query = Query(createDBObject(q.m), skip, limit)
}