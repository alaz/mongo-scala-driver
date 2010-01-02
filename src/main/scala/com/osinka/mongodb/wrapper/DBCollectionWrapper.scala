package com.osinka.mongodb.wrapper

import com.mongodb.{DBCollection, DBObject}

trait DBCollectionWrapper {
    /**
     * Actual DBCollection object behind
     */
    val underlying: DBCollection

    protected def find(dbo: DBObject) = underlying find dbo
    protected def findOne(dbo: DBObject) = underlying findOne dbo
    protected def getCount(dbo: DBObject) = underlying getCount dbo

    /**
     * Mongo collection name
     */
    def getName = underlying.getName

    def getFullName = underlying.getFullName

    def drop: Unit = underlying.drop

    override def equals(obj: Any) = obj match {
        case other: DBCollectionWrapper => underlying.equals(other.underlying)
        case _ => false
    }
}