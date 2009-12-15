package com.osinka.mongodb.shape

import com.mongodb.DBObject
import com.osinka.mongodb._
import Preamble.{tryo, EmptyConstraints, pfToOption, dotNotation}
import wrapper.MongoCondition

/**
 * Stackable field serializer model
 *
 * Let's define field serializers and stack them like
 *   opt + defl
 * or
 *   opt + emb(objectShape)
 */
trait FieldSerializer[A, R] { self =>
    val from: PartialFunction[R, A]
    val to:   PartialFunction[A, R]

    def +[T](fs: FieldSerializer[R, T]) = new FieldSerializer[A,T] {
        val to = self.to andThen fs.to
        val from = fs.from andThen self.from
    }
}

/**
 * Basic field
 */
trait Field[Host, A] extends BaseShape {
    /**
     * Field name
     */
    def fieldName: String

    /**
     * Is this field MongoDB-internal?
     */
    def mongo_? : Boolean = fieldName startsWith "$"

    private[shape] def valueOf(x: Host): Option[Any] = pack(getter(x))

    /**
     * How to get a field value from object?
     */
    def getter: Host => A

    /**
     * Field serializer
     */
    val serializer: FieldSerializer[A, Any]

    /* serialize/deserialize methods */
    private[shape] def extract(x: Any): Option[A] = pfToOption(serializer.from)(x)
    private[shape] def pack(v: A): Option[Any] = pfToOption(serializer.to)(v)
}

/**
 * field can update host object from DBObject's value
 */
trait HostUpdate[Host, A] { self: Field[Host, A] =>
    /*
     * v is non-null
     */
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

    // default serializer for scalar ordinary fields
    implicit def defl[A] = new FieldSerializer[A, Any] {
        val from: PartialFunction[Any, A] = {
            // avoiding type erasure warning
            case v /*if v.isInstanceOf[A]*/ => v.asInstanceOf[A]
        }
        val to: PartialFunction[A, Any] = { case x => x }
    }

    // serializer for Option[A] fields
    def opt[A] = new FieldSerializer[Option[A],A] {
        val from: PartialFunction[A,Option[A]] = { case x => Some(x) }
        val to: PartialFunction[Option[A],A]   = { case Some(x) => x }
    }

    // serializer for embedded objects
    def emb[V](o: ObjectIn[V, _]) = new FieldSerializer[V, Any] {
        // Looking for nicer way:
        // http://stackoverflow.com/questions/1908295/how-to-convert-x-optionr-to-partialfunctionx-r
        object extractor {
            def unapply(v: Any): Option[V] =
                if (v.isInstanceOf[DBObject]) o.out(v.asInstanceOf[DBObject])
                else None
        }

        val from: PartialFunction[Any,V] = { case extractor(x) => x }
        val to: PartialFunction[V, Any] = { case x => o.in(x) }
    }

    // TODO: ref serializer
    // TODO: array serializer

    /**
     * Scalar field. Can be instantiated as an object or variable.
     */
    class Scalar[A](override val fieldName: String, override val getter: Host => A)(implicit s: FieldSerializer[A, Any])
            extends Field[Host, A] with FieldCond[Host, QueryType, A] {

        override val fieldPath = parent.fieldPath ::: super.fieldPath
        override val serializer = s

        override def constraints = Map( MongoCondition.exists(mongoFieldName, true) )
    }

    object Scalar {
        /**
         * Scalar instantiation helper. With getter only
         */
        def apply[A](fieldName: String, getter: Host => A)(implicit s: FieldSerializer[A, Any]) =
            new Scalar[A](fieldName, getter)(s) with Functional[A]

        /**
         * Scalar instantiation helper. With getter and setter
         */
        def apply[A](fieldName: String, getter: Host => A, setter: (Host, A) => Unit)(implicit s: FieldSerializer[A, Any]) =
            new Scalar[A](fieldName, getter)(s) with Functional[A] with Updatable[A] {
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
        override val serializer: FieldSerializer[V, Any] = emb(objectShape)

        override def constraints =
            (EmptyConstraints /: objectShape.constraints) { (m, e) =>
                m + (dotNotation(fieldPath ::: e._1 :: Nil) -> e._2)
            }
    }

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
        def unapply(dbo: DBObject): Option[A] = tryo(dbo get fieldName) flatMap extract
    }
}