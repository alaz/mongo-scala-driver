package com.osinka.mongodb

import com.mongodb._
import Helper._

trait MutableCollection[T] extends ReadonlyCollection[T] {
    val in: PartialFunction[T, DBObject]

    // TODO: return T

    def insert(x: T): Option[DBObject] = pfToOption(in andThen underlying.insert)(x)

    def insert_?(x: T): Option[DBObject] = pfToOption(in)(x) flatMap { obj =>
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    def save(x: T): Option[DBObject] = pfToOption(in andThen underlying.save)(x)

    def remove(x: T) { (in andThen underlying.remove)(x) }
}

class DBObjectMutableCollection(override val underlying: DBCollection) extends DBObjectReadonlyCollection(underlying) with MutableCollection[DBObject] with DBCollectionWrapper {
    override val in: PartialFunction[DBObject, DBObject] = { case x => x }

    override def insert(o: DBObject): Option[DBObject] = Some(underlying.insert(o))

    override def insert_?(obj: DBObject): Option[DBObject] = {
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    override def save(obj: DBObject): Option[DBObject] = Some(underlying.save(obj))

    override def remove(obj: DBObject) { underlying.remove(obj) }

//    override def update {}
}