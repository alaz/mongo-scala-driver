package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

sealed trait Query {
    def query: DBObject
    def skip: Int
    def limit: Int

    def ++(q: DBObject) = {
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
}

trait QueriedCollection[+T] extends ReadonlyCollection[T] {
    def query: Query
    
    override def find = super.find(query)
}


/*
CollObject.where{_.fieldName < 2}

Query(CollObject).where{c => c.field < 2).drop(10).take(20).findAllIn(coll)
Query(CollObject).findFirstIn(coll)
*/