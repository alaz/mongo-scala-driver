package com.osinka.mongodb.shape

import com.mongodb.{DBObject, BasicDBObjectBuilder}
import Helper._

trait BaseField[A, +FS] extends Transformer[A, Any] with BaseShape[FS] {
    def fieldName: String
    def mongo_? : Boolean = fieldName startsWith "$"
}

trait EmbeddableField { self: BaseField[_, _] =>
    private[shape] def fieldPath: List[String] = fieldName :: Nil

    private[shape] lazy val mongoFieldName = fieldPath.mkString(".")
}

trait FieldContainer {
    private[shape] def fieldPath: List[String] = Nil
}

abstract case class Field[Host, A, +FS](override val fieldName: String, val getter: Host => A)
        extends BaseField[A, FS] {

    private[shape] def valueOf(x: Host): Any = pack(getter(x))
}

/**
 * field can update host object from DBObject's value
 */
trait HostUpdate[Host, A] { self: BaseField[A, _] =>
    private[shape] def updateUntyped(x: Host, v: Any): Unit = extract(v) foreach { update(x, _) }

    /**
     * Update is not mandatory, but field will need it to modify host object
     * with the new field value.
     */
    def update(x: Host, v: A): Unit
}

trait ShapeFields[Host, QueryType] extends FieldContainer { parent =>
    case class Scalar[A](override val fieldName: String, override val getter: Host => A)
            extends Field[Host, A, Int](fieldName, getter) with FieldCond[Host, QueryType, A] {

        override val fieldPath = parent.fieldPath ::: super.fieldPath
        override val shape: Int = 1
        override def extract(v: Any): Option[A] = tryo(v) map {_.asInstanceOf[A]}
        override def pack(v: A): Any = v
    }

    case class Embedded[V](override val fieldName: String, val element: DBObjectShape[V], override val getter: Host => V)
            extends Field[Host, V, DBObject](fieldName, getter) with FieldContainer {

        override val fieldPath = parent.fieldPath ::: fieldName :: Nil
        override val shape: DBObject = element.shape
        override def extract(v: Any): Option[V] =
            for {val raw <- tryo(v) if raw.isInstanceOf[DBObject]
                 val dbo = raw.asInstanceOf[DBObject]
                 val result <- element extract dbo}
            yield result
        override def pack(v: V): Any = element.pack(v)
    }

    // TODO: ref
    // TODO: array

    /**
     * Field can be updated
     */
    trait Updatable[A] extends HostUpdate[Host, A] { self: Field[Host, A, _] => }

    /**
     * Internal mongo field. always scalar
     */
    trait Mongo[A] extends BaseField[A, Int] { self: Scalar[A] =>
        override def mongo_? = true
    }

    /**
     * Support for unapplying parent's DBO
     */
    trait Functional[A] { self: Field[Host, A, _] =>
        def unapply(dbo: DBObject): Option[A] = extract(dbo get fieldName)
    }
}