package com.osinka.mongodb.shape

import scala.reflect.Manifest
import com.mongodb._
import Helper._

trait GetAndSet[Host, Field] {
    def apply(x: Host): Field
    def update(x: Host, v: Field): Unit

    private[shape] def getter(x: Host): Any = apply(x)
    private[shape] def setter(x: Host, v: Any): Unit = { update(x, v.asInstanceOf[Field]) }
}

/*
trait Readonly[Host, Field] extends GetAndSet[Host, Field] {
    override def apply(x: Host): Option[Field] = None
}

trait Writeonly[Host, Field] extends GetAndSet[Host, Field] {
    override def update(x: Host, v: Field): Unit = {}
}
*/

trait BaseShape[Host, S] {
    val shape: S

    abstract case class field[A, FS](val name: String)
            extends BaseShape[A, FS] with GetAndSet[Host, A]

    abstract case class scalar[A](override val name: String) extends field[A, Int](name) {
        override val shape: Int = 1
    }

/*    case class fnField[A](override val name: String,
                          val get: Host => A,
                          val set: (Host, A) => Unit)
            extends scalar[A](name) {

        override def apply(x: Host): A = get(x)
        override def update(x: Host, v: A) = set(x, v)
    }*/

    abstract case class nested[V, O <: DBObjectShape[V]](override val name: String, val element: DBObjectShape[V])
            extends field[V, DBObject](name) {

        override val shape: DBObject = element.shape

        override def getter(x: Host): Any = {
            val dbo = BasicDBObjectBuilder.start.get
            element.update(dbo, apply(x))
            dbo
        }
        
        override def setter(x: Host, v: Any): Unit = {
            update(x, element.apply(v.asInstanceOf[DBObject]))
        }
    }

    // TODO: ref
    // TODO: array

    // TODO: Bean-based field, Annotation-based shape, etc.
}

trait DBObjectShape[T] extends BaseShape[T, DBObject] with GetAndSet[DBObject, T]

trait MongoObjectShape[T <: MongoObject] extends DBObjectShape[T] {
    def clazz: Class[_]
    def * : List[field[_, _]] = oid :: ns :: Nil

    lazy val shape: DBObject = {
        import scala.collection.immutable.{Map, Set}

        def internal_?(f: field[_, _]) = {
            val predef_? = Set(oid.name, ns.name)
            f match {
                case _ if predef_?(f.name) => true
                case _ if f.name startsWith "$" => true
                case _ => false
            }
        }

        val emptyMap = Map[String,Any]()
        Preamble.createDBObject( (* remove internal_? foldLeft emptyMap) {(m, f) => m + (f.name -> f.shape)} )
    }

    def factory(obj: DBObject): T = clazz.asInstanceOf[Class[T]].newInstance

    override def apply(dbo: DBObject) = {
        val x = factory(dbo)
        for {val f <- *
             val v = dbo.get(f.name) } f.setter(x, dbo.get(f.name))
        x
    }

    override def update(dbo: DBObject, x: T) {
        for {val f <- * }
            dbo.put(f.name, f.getter(x))
    }

    object oid extends scalar[ObjectId]("_id") {
        override def apply(x: T) = x.mongoOID
        override def update(x: T, v: ObjectId) { x.mongoOID = v }
    }

    object ns extends scalar[String]("_ns") {
        override def apply(x: T) = x.mongoNS
        override def update(x: T, v: String) { x.mongoNS = v }
    }
}

class Shape[T <: MongoObject](implicit m: Manifest[T]) extends MongoObjectShape[T] {
    override val clazz = m.erasure
}