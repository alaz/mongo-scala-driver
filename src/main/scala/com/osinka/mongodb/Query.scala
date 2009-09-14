package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

case class Query(val query: Option[DBObject], val skip: Int, val limit: Int)
case object EmptyQuery extends Query(None, 0, Query.NoLimit)

object Query {
    val Empty = new BasicDBObject
    val NoLimit = -1
}

/*
CollObject.where{_.fieldName < 2}

Query(CollObject).where{c => c.field < 2).drop(10).take(20).findAllIn(coll)
Query(CollObject).findFirstIn(coll)
*/