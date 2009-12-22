package com.osinka.mongodb.shape

import scala.reflect.Manifest
import com.mongodb.DBObject
import Preamble.{tryo, EmptyConstraints}
import wrapper.DBO

/*
 * Basic object/field shape
 */
trait BaseShape {
    /**
     * Constraints on collection object to have this "shape"
     */
    def mongoConstraints: Map[String, Map[String, Boolean]]
}

trait ObjectFieldReader[T] {
    private[shape] def mongoReadFrom(x: T): Option[Any]
}

trait ObjectField[T] extends BaseShape with ObjectFieldReader[T] {
    def mongoFieldName: String
    def mongoInternal_? : Boolean = mongoFieldName startsWith "_"
}

trait ObjectFieldWriter[T] { self: ObjectField[T] =>
    private[shape] def mongoWriteTo(x: T, v: Option[Any])
}

/*
 * Shape of an object held in some other object (being it a Shape or Query)
 */
trait ObjectIn[T, QueryType] extends BaseShape with Serializer[T] with ShapeFields[T, QueryType] {
    def * : List[ObjectField[T]]
    def factory(dbo: DBObject): Option[T]

    protected def fieldList: List[ObjectField[T]] = *

    private[shape] def packFields(x: T, fields: Seq[ObjectField[T]]): DBObject =
        DBO.fromMap( (fields foldLeft Map[String,Any]() ) { (m,f) =>
            assert(f != null, "Field must not be null")
            f.mongoReadFrom(x) match {
                case Some(v) => m + (f.mongoFieldName -> v)
                case None => m
            }
        } )

    private[shape] def updateFields(x: T, dbo: DBObject, fields: Seq[ObjectField[T]]) {
        for {f <- fields if f.isInstanceOf[ObjectFieldWriter[_]]
             updatableField = f.asInstanceOf[ObjectFieldWriter[T]] }
            updatableField.mongoWriteTo(x, tryo(dbo get f.mongoFieldName))
    }

    // -- BaseShape[T,R]
    override lazy val mongoConstraints = (fieldList remove {_.mongoInternal_?} foldLeft EmptyConstraints) { (m,f) =>
        assert(f != null, "Field must not be null")
        m ++ f.mongoConstraints
    }

    // -- Serializer[T]
    override def in(x: T): DBObject = packFields(x, fieldList)

    override def out(dbo: DBObject) = factory(dbo) map { x =>
        assert(x != null, "Factory should not return Some(null)")
        updateFields(x, dbo, fieldList)
        x
    }

    override def mirror(x: T)(dbo: DBObject) = {
        assert(x != null, "Mirror should not be called on null")
        updateFields(x, dbo, fieldList filter { _.mongoInternal_? })
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

//    lazy val oid = Field.scalar("_id", (x: T) => x.mongoOID, (x: T, oid: ObjectId) => x.mongoOID = oid)
//    lazy val ns = Field.scalar("_ns", (x: T) => x.mongoNS, (x: T, ns: String) => x.mongoNS = ns)

    object oid extends ScalarField[ObjectId]("_id", (x: T) => x.mongoOID, Some( (x: T, oid: ObjectId) => x.mongoOID = oid) )
            with Functional[ObjectId] with Optional[ObjectId]

    object ns extends ScalarField[String]("_ns", (x: T) => x.mongoNS, Some( (x: T, ns: String) => x.mongoNS = ns) )
            with Functional[String] with Optional[String]

    // -- ObjectShape[T]
    override def fieldList : List[ObjectField[T]] = oid :: ns :: super.fieldList
}