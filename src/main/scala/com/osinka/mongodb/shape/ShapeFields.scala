package com.osinka.mongodb.shape

import com.mongodb.DBObject
import Preamble.{tryo, EmptyConstraints}

trait Field[Host, A] extends BaseShape {
    def fieldName: String
    def getter: Host => A

    def mongo_? : Boolean = fieldName startsWith "$"

    /* serialize/deserialize methods */
    def extract(x: Any): Option[A] // a kind of "unapply"
    def pack(v: A): Option[Any] // a kind of "apply"

    protected def dotNotation(l: List[String]) = l.mkString(".")
    private[shape] def valueOf(x: Host): Option[Any] = pack(getter(x))
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
    trait FieldSerializer[A] {
        def optional: Boolean = false
        def pack(x: A): Option[Any]
        def extract(v: Any): Option[A]
    }

    implicit def defaultFieldSerializer[A] = new FieldSerializer[A] {
        override def pack(x: A): Option[Any] = Some(x)
        override def extract(v: Any): Option[A] = tryo(v) flatMap {x => Some(x.asInstanceOf[A])}
    }

    implicit def optionFieldSerializer[A] = new FieldSerializer[Option[A]] {
        override def optional: Boolean = true
        override def pack(x: Option[A]): Option[Any] = x
        override def extract(v: Any): Option[Option[A]] = tryo(v) map {x => Some(x.asInstanceOf[A])}
    }
*/

    /**
     * Scalar field. Can be instantiated as an object or variable.
     */
    class Scalar[A](override val fieldName: String, override val getter: Host => A)
            extends Field[Host, A] with FieldCond[Host, QueryType, A] {

        override val fieldPath = parent.fieldPath ::: super.fieldPath

        override def constraints = Map( MongoCondition.exists(mongoFieldName, true) )
        override def pack(v: A): Option[Any] = Some(v)
        override def extract(v: Any): Option[A] = tryo(v) flatMap {x => Some(x.asInstanceOf[A])}
    }

    object Scalar {
        /**
         * Scalar instantiation helper. With getter only
         */
        def apply[A](fieldName: String, getter: Host => A) =
            new Scalar[A](fieldName, getter) with Functional[A]

        /**
         * Scalar instantiation helper. With getter and setter
         */
        def apply[A](fieldName: String, getter: Host => A, setter: (Host, A) => Unit) =
            new Scalar[A](fieldName, getter) with Functional[A] with Updatable[A] {
                override def update(host: Host, v: A) { setter(host, v) }
            }
    }

    /**
     * Shape object living in a field.
     *
     * For instantiation as an object or variable. ObjectIn should be mixed in.
     */
    class Embedded[V](override val fieldName: String, override val getter: Host => V) extends Field[Host, V] with FieldContainer {
        objectShape: ObjectIn[V, Host] =>

        override val fieldPath = parent.fieldPath ::: fieldName :: Nil

        override def constraints =
            (EmptyConstraints /: objectShape.constraints) { (m, e) =>
                m + (dotNotation(fieldPath ::: e._1 :: Nil) -> e._2)
            }
        override def pack(v: V): Option[Any] = Some(objectShape.in(v))
        override def extract(v: Any): Option[V] =
            for {val raw <- tryo(v) if raw.isInstanceOf[DBObject]
                 val dbo = raw.asInstanceOf[DBObject]
                 val result <- objectShape out dbo}
            yield result
    }

    // TODO: ref
    // TODO: array

    /**
     * Field can be updated
     */
    trait Updatable[A] extends HostUpdate[Host, A] { self: Field[Host, A] => }

    /*
     * Optional field
     */
    trait Optional[A] extends Field[Host, A] {
        override def constraints = EmptyConstraints
    }

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