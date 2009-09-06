package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject}

case class Query(val query: Option[DBObject], val skip: Int, val limit: Int)
case object EmptyQuery extends Query(None, 0, Query.NoLimit)

object Query {
    val DefaultQuery = new BasicDBObject
    val NoLimit = -1
}

/*
*/