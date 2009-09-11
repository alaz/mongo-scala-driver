package com.osinka.mongodb

import com.mongodb.DBCollection

trait DBCollectionWrapper {
    val underlying: DBCollection

    def getName = underlying.getName

    def getFullName = underlying.getFullName

    def drop {
        underlying.drop
    }

    override def equals(obj: Any) = obj match {
        case other: DBCollectionWrapper => underlying.equals(other.underlying)
        case _ => false
    }
}