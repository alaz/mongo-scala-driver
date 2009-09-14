package com.osinka.mongodb

import com.mongodb._

object H {
    def apply[A, B](f: PartialFunction[A,B])(a: A) =
        if (f.isDefinedAt(a)) Some(f(a))
        else None
}

trait MutableCollection[T] extends ImmutableCollection[T] {
    val in: PartialFunction[T, DBObject]

    // TODO: return T

    def insert(x: T): Option[DBObject] = H(in andThen underlying.insert)(x)

    def insert_?(x: T): Option[DBObject] = H(in)(x) flatMap { obj =>
        val r = underlying insert obj
        underlying.getBase.getLastError get "err" match {
            case null => Some(r)
            case msg: String => None
        }
    }

//    def insertAll(objs: Seq[DBObject]): List[DBObject] = List(underlying insert objs.toArray[DBObject])

    def save(x: T): Option[DBObject] = H(in andThen underlying.save)(x)

    def remove(x: T) { (in andThen underlying.remove)(x) }
}

class DBObjectMutableCollection(override val underlying: DBCollection) extends DBObjectImmutableCollection(underlying) with MutableCollection[DBObject] with DBCollectionWrapper {
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