package com.osinka.mongodb.shape

import scala.reflect.Manifest
import com.mongodb._
import Helper._

trait BaseShape[T, S] {
    val shape: S

    abstract class field[A](val name: String) extends (T => A) with BaseShape[A, Int] {
        override val shape: Int = 1
        def apply(x: T): A
        def update(x: T, v: A): Unit
    }

    case class fnField[A](override val name: String, val get: T => A, val set: ((T, A) => Unit)) extends field[A](name) {
        def apply(x: T): A = get(x)
        def update(x: T, v: A): Unit = set(x, v)
    }

    // TODO: Bean-based field, Annotation-based shape, etc.
}

class Shape[T <: MongoObject](implicit m: Manifest[T]) extends BaseShape[T, DBObject] {
    def factory(obj: DBObject): T = m.erasure.asInstanceOf[Class[T]].newInstance

    lazy val shape: DBObject = {
        import scala.collection.immutable.Map

        val emptyMap = Map[String,Any]()
        Preamble.createDBObject( (* foldLeft emptyMap) {(m, f) => m + (f.name -> f.shape)} )
    }

    object oid extends field[ObjectId]("_id") {
        def apply(x: T): ObjectId = x.mongoOID
        def update(x: T, v: ObjectId): Unit = x.mongoOID = v
    }

    object ns extends field[String]("_ns") {
        def apply(x: T): String = x.mongoNS
        def update(x: T, v: String): Unit = x.mongoNS = v
    }

    def * : List[field[_]] = oid :: ns :: Nil
}
