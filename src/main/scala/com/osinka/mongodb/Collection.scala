package com.osinka.mongodb

import com.mongodb.{DBCollection, DBObject}
import Helper._

trait OriginProxy {
    def underlying: DBCollection
}

trait CollectionAdmin extends OriginProxy {
    // TODO: indexes
}

trait SchemafreeCollection extends OriginProxy { self =>
    type MyType <: SchemafreeCollection

    // Modifiers
    def insert[T <: AnyRef](obj: T): Option[T] = serializer.provide(obj).map{o => serializer.merge(obj, underlying.insert(o))}

    def insert[T <: AnyRef](objs: Seq[T]): List[T] = throw new Exception /*{
        val dbos = objs.flatMap{o => serializer provide o}
        val inserted = underlying.insert(dbos.toArray)
        (objs.toList zip inserted.toList) map {x => serializer.merge(x._1, x._2)}
    }*/

    // Query
    def firstOption[T](f: DBObject => T): Option[T] = tryo(underlying.findOne).map{f(_)}

    def firstOption[T](f: PartialFunction[DBObject, T]): Option[T] = tryo(underlying.findOne).filter{f.isDefinedAt(_)}.map{f(_)}

    def elements[T](f: DBObject => T): Iterator[T] = Iterator.empty

    def skip(n: Int): MyType = factory(Query(query.query, query.skip+n, query.limit))

    def limit(n: Int): MyType = factory(Query(query.query, query.skip, n))

    def size: Long = query.query.map{underlying.getCount(_)}.getOrElse(underlying.getCount)

    protected def serializer: Serializer
    protected def query: Query = EmptyQuery

    protected def factory(q: Query): MyType = new SchemafreeCollection {
        override type MyType = AnyRef with SchemafreeCollection
        override val underlying = self.underlying
        override val serializer = self.serializer
        override def query = q
    }
}

/* Typed:

trait MongoObject {
    val _id: String
    val _ns: String
}

trait MongoCollection[T <: forSome {_id; _ns}] extends SchemafreeColletion with Seq[T] {
    def length

    def headOption

    def firstOption

    def elements: Iterator[T]
}

trait Field[T] {
    def <(val: T) = FieldQuery(new BasicDBObject(name, new BasicDBObject("$lt", val)))
    def &&[X](q: FieldQuery) = FieldQuery
}

DSL:

coll skip 10 limit 5
coll << obj1 << obj2
coll ? (field("field") == 1)
coll ? (field("field1") \ field("field2") < 2)

coll ? (Type.field1 \ Type.field2 >= "value")
*/

class OriginWrapper(override val underlying: DBCollection) extends SchemafreeCollection with CollectionAdmin { self =>
    override type MyType = OriginWrapper
    
    protected override val serializer: Serializer = Serializer

    protected override def factory(q: Query) = new OriginWrapper(self.underlying) {
        protected override val query = q
    }
}

/*
trait CollectionWrapper[+A] extends CollectionUpdates[A] with Iterable[A] with Serializer {
    def size: Long // TODO: take Query into account
    def elements: Iterator[A] // TODO: Iterable.iterator

    def find: CollectionWrapper[A]
    def findOne(): Option[A]

    def find[B <: A](prototype: B): CollectionWrapper[B]

}

class OriginWrapper[+A](override val underlying: DBCollection) extends CollectionWrapper[A] with CollectionAdmin with OriginProxy {
    def size: Long = underlying.getCount
}

trait TypedCollectionWrapper[+A] extends CollectionWrapper[A] with TypedSerializer[A] {
    def indexes: CollectionWrapper[Index[A]]
}
*/