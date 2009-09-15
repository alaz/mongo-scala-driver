package com.osinka.mongodb

import com.mongodb._

trait Builder[T] {
    def mutable(dboColl: DBCollection): MutableCollection[T]

    def mutable(c: ReadonlyCollection[T]): MutableCollection[T] = error("") /*new MutableCollection[T] {
        override val underlying = c.underlying
    }*/

//    def immutable(c: ImmutableCollection[T], q: Query): ImmutableCollection[T]
//    def mutable(c: ImmutableCollection[T], q: Query): MutableCollection[T]
}

object PlainDBOBuilder extends Builder[DBObject] {
    override def mutable(dboColl: DBCollection): MutableCollection[DBObject] = new DBObjectMutableCollection(dboColl)

    override def mutable(c: ReadonlyCollection[DBObject]): MutableCollection[DBObject] = new DBObjectMutableCollection(c.underlying)

//    override def immutable(c: ImmutableCollection[DBObject], q: Query): ImmutableCollection[DBObject]
//    override def mutable(c: ImmutableCollection[DBObject], q: Query): MutableCollection[DBObject]
}