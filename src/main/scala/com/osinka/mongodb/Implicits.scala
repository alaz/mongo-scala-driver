/*
 * Implicits.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.osinka.mongodb

import com.mongodb.{DBObject, DBCollection}

trait Implicits {
    import wrapper._

    implicit def collAsScala(coll: DBCollection) = new {
        def asScala = new DBObjectCollection(coll)
    }

    implicit def queryToColl(q: Query) = new {
        def in[T, Self <: QueriedCollection[T, Self]](coll: QueriedCollection[T, Self]): Self = coll.applied(q)
    }

    implicit def wrapperToDBO(coll: DBCollectionWrapper): DBCollection = coll.underlying

    implicit def mapToDBObject(m: Map[String, Any]): DBObject = DBO.fromMap(m)
}
