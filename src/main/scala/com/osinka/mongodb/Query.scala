package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

sealed trait Query {
    def query: DBObject
    def skip: Int
    def limit: Int

    def ++(q: DBObject): Query = {
        val dbo = new BasicDBObject
        dbo putAll query
        dbo putAll q
        Query(dbo)
    }

    def drop(n: Int) = Query(query, skip+n, limit)

    def take(n: Int) = Query(query, skip, n)
}

case class NonemptyQuery (override val query: DBObject, override val skip: Int, override val limit: Int) extends Query {
}

case object EmptyQuery extends Query {
    override val query = Query.Empty
    override val skip = 0
    override val limit = Query.NoLimit
}

object Query {
    val Empty = new BasicDBObject
    val NoLimit = -1

    def apply() = EmptyQuery
    def apply(q: DBObject) = NonemptyQuery(q, 0, NoLimit)
    def apply(q: DBObject, s: Int, l: Int) = NonemptyQuery(q, s, l)

//    case class Term[+T] {
//        def in(r: Range): Term[T]
//
//        def in(seq: Seq[T]): Term[T]
//    }
}

trait QueriedCollection[T] extends MongoCollection[T] {
    def query: Query
    
    override def find(q: Query) = super.find(q ++ query.query)
    override def getCount(q: Query) = super.getCount(q ++ query.query)
}


/*
CollObject.where{_.fieldName < 2}

Query(CollObject).where{c => c.field < 2).drop(10).take(20).findAllIn(coll)
Query(CollObject).findFirstIn(coll)
*/