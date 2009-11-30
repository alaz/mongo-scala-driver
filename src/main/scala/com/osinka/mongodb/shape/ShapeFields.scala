package com.osinka.mongodb.shape

import com.mongodb.DBObject
import Preamble.tryo

trait Field[Host, A] extends BaseShape[A, Any] {
    def fieldName: String
    def getter: Host => A

    def mongo_? : Boolean = fieldName startsWith "$"

    protected def dotNotation(l: List[String]) = l.mkString(".")
    private[shape] def valueOf(x: Host): Any = pack(getter(x))
}

/**
 * field can update host object from DBObject's value
 */
trait HostUpdate[Host, A] { self: Field[Host, A] =>
    private[shape] def updateUntyped(x: Host, v: Any): Unit = extract(v) foreach { update(x, _) }

    /**
     * Update is not mandatory, but field will need it to modify host object
     * with the new field value.
     */
    def update(x: Host, v: A): Unit
}

trait FieldContainer {
    private[shape] def fieldPath: List[String] = Nil
}

trait ShapeFields[Host, QueryType] extends FieldContainer { parent =>

    /*
    implicit object intExtractor extends FieldExtractor[Int] {
        def extract(v: Any): Option[Int] = v match {
            case d: Double => Some(d.toInt)
        }
    }
    */

    case class Scalar[A](override val fieldName: String, override val getter: Host => A) /*(implicit extractor: FieldExtractor[A])*/
            extends Field[Host, A] with FieldCond[Host, QueryType, A] {

        override val fieldPath = parent.fieldPath ::: super.fieldPath

        override lazy val constraints = Map( MongoCondition.exists(mongoFieldName, true) )
        override def pack(v: A): Any = v
        override def extract(v: Any): Option[A] = tryo(v) map {_.asInstanceOf[A]}
    }

    case class Embedded[V](override val fieldName: String, val objectShape: ObjectShape[V], override val getter: Host => V)
            extends Field[Host, V] with FieldContainer {

        override val fieldPath = parent.fieldPath ::: fieldName :: Nil

        override lazy val constraints =
            (Map.empty[String, Map[String, Boolean]] /: objectShape.constraints) { (m, e) =>
                m + (dotNotation(fieldPath ::: e._1 :: Nil) -> e._2)
            }
        override def pack(v: V): Any = objectShape.pack(v)
        override def extract(v: Any): Option[V] =
            for {val raw <- tryo(v) if raw.isInstanceOf[DBObject]
                 val dbo = raw.asInstanceOf[DBObject]
                 val result <- objectShape extract dbo}
            yield result
    }

    // TODO: ref
    // TODO: array

    /**
     * Field can be updated
     */
    trait Updatable[A] extends HostUpdate[Host, A] { self: Field[Host, A] => }

    /**
     * Internal mongo field. always scalar
     */
    trait Mongo[A] extends Field[Host, A] { self: Scalar[A] =>
        override def mongo_? = true
    }

    /**
     * Support for unapplying parent's DBO
     */
    trait Functional[A] { self: Field[Host, A] =>
        def unapply(dbo: DBObject): Option[A] = extract(dbo get fieldName)
    }
}