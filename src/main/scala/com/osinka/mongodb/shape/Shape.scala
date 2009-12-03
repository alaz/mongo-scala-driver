package com.osinka.mongodb.shape

import scala.reflect.Manifest
import com.mongodb.DBObject
import Preamble.tryo
import wrapper.DBO

/*
 * Basic object/field shape
 */
trait BaseShape {
    /**
     * Constraints on collection object to have this "shape"
     */
    def constraints: Map[String, Map[String, Boolean]]
}

/*
 * Shape of an object held in some other object (being it a Shape or Query)
 */
trait ObjectIn[T, QueryType] extends BaseShape with Serializer[T] with ShapeFields[T, QueryType] {
    def * : List[Field[T, _]]
    def factory(dbo: DBObject): Option[T]

    protected def fieldList: List[Field[T, _]] = *

    // -- BaseShape[T,R]
    override lazy val constraints = (fieldList remove {_.mongo_?} foldLeft Map[String,Map[String,Boolean]]() ) { (m,f) =>
        assert(f != null, "Field must not be null")
        m ++ f.constraints
    }

    // -- Serializer[T]
    override def out(dbo: DBObject) = factory(dbo) map { x =>
        assert(x != null, "Factory should not return Some(null)")
        for {f <- fieldList if f.isInstanceOf[HostUpdate[_,_]]
             updatableField = f.asInstanceOf[HostUpdate[T,_]]
             fieldDbo <- tryo(dbo get f.fieldName)}
            updatableField.updateUntyped(x, fieldDbo)
        x
    }

    override def in(x: T): DBObject =
        DBO.fromMap( (fieldList foldLeft Map[String,Any]() ) { (m,f) =>
            assert(f != null, "Field must not be null")
            f.valueOf(x) match {
                case Some(v) => m + (f.fieldName -> v)
                case None => m
            }
        } )

    override def mirror(x: T)(dbo: DBObject) = {
        for {f <- fieldList if f.mongo_? && f.isInstanceOf[HostUpdate[_,_]]
             updatableField = f.asInstanceOf[HostUpdate[T,_]]
             fieldDbo <- tryo(dbo get f.fieldName)}
            updatableField.updateUntyped(x, fieldDbo)
        x
    }
}

/*
 * Shape of an object backed by DBObject ("hosted in")
 */
trait ObjectShape[T] extends ObjectIn[T, T] with Queriable[T]

/**
 * Mix-in to make a shape functional, see FunctionalTransformer for explanation
 *
 * FunctionalShape make a shape with convinient syntactic sugar
 * for converting object to DBObject (apply) and extractor for the opposite
 *
 * E.g.
 * val dbo = UserShape(u)
 * dbo match {
 *    case UserShape(u) =>
 * }
 */
trait FunctionalShape[T] { self: ObjectShape[T] =>
    def apply(x: T): DBObject = in(x)
    def unapply(rep: DBObject): Option[T] = out(rep)
}

/**
 * Shape of MongoObject child.
 *
 * It has mandatory _id and _ns fields
 */
trait MongoObjectShape[T <: MongoObject] extends ObjectShape[T] {
    import com.mongodb.ObjectId

    object oid extends Scalar[ObjectId]("_id", _.mongoOID)
            with Functional[ObjectId] with Mongo[ObjectId] with Updatable[ObjectId] {
        override def update(x: T, oid: ObjectId) { x.mongoOID = oid }
    }
    object ns extends Scalar[String]("_ns", _.mongoNS)
            with Functional[String] with Mongo[String] with Updatable[String] {
        override def update(x: T, ns: String) { x.mongoNS = ns }
    }

    // -- ObjectShape[T]
    override def fieldList : List[Field[T, _]] = oid :: ns :: super.fieldList
}