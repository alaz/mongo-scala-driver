package com.osinka.mongodb.shape

import com.mongodb.{DBObject, BasicDBObjectBuilder}
import Helper._

trait BaseField[A, +FS] extends Transformer[A, Any] with BaseShape[FS] {
    def name: String
    def mongo_? : Boolean = name startsWith "$"
}

abstract case class Field[Host, A, +FS](override val name: String, val getter: Host => A)
        extends BaseField[A, FS] with FieldCond[Host, A] {

    private[shape] def valueOf(x: Host): Any = pack(getter(x))
}

/**
 * field can update host object from DBObject's value
 */
trait HostUpdate[Host, A] { self: BaseField[A, _] =>
    private[shape] def updateUntyped(x: Host, v: Any): Unit = extract(v) map { update(x, _) }

    /**
     * Update is not mandatory, but field will need it to modify host object
     * with the new field value.
     */
    def update(x: Host, v: A): Unit
}

trait ShapeFields[Host] {

    case class scalar[A](override val name: String, override val getter: Host => A)
            extends Field[Host, A, Int](name, getter) {

        override val shape: Int = 1
        override def extract(v: Any): Option[A] = tryo(v) map {_.asInstanceOf[A]}
        override def pack(v: A): Any = v
    }

    case class nested[V](override val name: String, val element: DBObjectShape[V], override val getter: Host => V)
            extends Field[Host, V, DBObject](name, getter) {

        override val shape: DBObject = element.shape
        override def extract(v: Any): Option[V] =
            ( tryo(v)
              filter {_.isInstanceOf[DBObject]}
              flatMap {element extract _.asInstanceOf[DBObject]} )
        override def pack(v: V): Any = element.pack(v)
    }

    // TODO: ref
    // TODO: array

    // TODO: Bean-based field, Annotation-based shape, etc.

    /**
     * Field can be updated
     */
    trait Updatable[A] extends HostUpdate[Host, A] { self: Field[Host, A, _] => }

    /**
     * Internal mongo field. always scalar
     */
    trait Mongo[A] extends BaseField[A, Int] { self: scalar[A] =>
        override def mongo_? = true
    }

    /**
     * Support for unapplying parent's DBO
     */
    trait Functional[A] { self: Field[Host, A, _] =>
        def unapply(dbo: DBObject): Option[A] = extract(dbo get name)
    }
}