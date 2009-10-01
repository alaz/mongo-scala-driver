package com.osinka.mongodb.shape

import scala.reflect.Manifest
import com.mongodb._
import Helper._

/*
 * Basic object shape
 */
trait BaseShape[Host, S] extends DBObjectStored[Host] with ShapeFields[Host] { /// not DBObjectStored
    val shape: S
}

/*
 * Shape of an object backed by DBObject ("hosted in")
 */
trait DBObjectShape[T] extends BaseShape[T, DBObject] {  /// extends DBObjectStored
    def * : List[Field[T, _, _]]
    def factory(dbo: DBObject): Option[T]

    override def extract(dbo: DBObject) = factory(dbo) map { x =>
        for {val f <- * if f.isInstanceOf[HostUpdate[_,_]]
             val v <- f.extract(dbo)}
            f.asInstanceOf[HostUpdate[T,_]].update(x, v)
        x
    }

    def pack(x: T): DBObject = {
        def merge(dbo: DBObject, v: DBObject) = {
            dbo.putAll(v)
            dbo
        }

        (* foldLeft emptyDBO) { (dbo, f) => merge(dbo, f valueOf x) }
    }

    lazy val shape: DBObject = {
        import scala.collection.immutable.{Map, Set}

        val emptyMap = Map[String,Any]()
        Preamble.createDBObject( (* remove {_.mongo_?} foldLeft emptyMap) {(m, f) => m + (f.name -> f.shape)} )
    }
}

/**
 * Shape of MongoObject child.
 *
 * It has mandatory _id and _ns fields
 */
trait MongoObjectShape[T <: MongoObject] extends DBObjectShape[T] {
    override def * : List[Field[T, _, _]] = oid :: ns :: Nil

    object oid extends scalar[ObjectId]("_id", _.mongoOID)
            with ShapeFunctionObject[ObjectId]
            with Mongo[ObjectId]
            with Updatable[ObjectId] {
        override def update(x: T, v: Any): Unit = v match {
            case oid: ObjectId => x.mongoOID = oid
        }
    }
    object ns extends scalar[String]("_ns", _.mongoNS)
            with ShapeFunctionObject[String]
            with Mongo[String]
            with Updatable[String] {
        override def update(x: T, v: Any): Unit = v match {
            case ns: String => x.mongoNS = ns
        }
    }
}


/*
 * Shape to be used by users.
 */
class Shape[T <: MongoObject](implicit m: Manifest[T]) extends MongoObjectShape[T] {
    val clazz = m.erasure
    override def factory(dbo: DBObject): Option[T] = Some(clazz.newInstance.asInstanceOf[T])
}