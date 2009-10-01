package com.osinka.mongodb.shape

import com.mongodb.{DBObject, BasicDBObjectBuilder}
import Helper._

trait BaseField[A, FS] extends BaseShape[A, FS] {
    def name: String
    def mongo_? : Boolean = name startsWith "$"
}

abstract case class Field[Host, A, FS](override val name: String) extends BaseField[A, FS] with (Host => A) {
    private[shape] def valueOf(x: Host): DBObject = pack(apply(x))
}

/**
 * field can update host object from DBObject's value
 */
trait HostUpdate[Host, A] {
    /**
     * Update is not mandatory, but field will need it to modify host object
     * with the new field value.
     */
    def update(x: Host, v: Any): Unit
}

trait ShapeFields[Host] {

    case class scalar[A](override val name: String, val getter: Host => A) extends Field[Host, A, Int](name) {
        override val shape: Int = 1

        def apply(x: Host): A = getter(x)

        def extract(dbo: DBObject): Option[A] = Helper.tryo( dbo.get(name).asInstanceOf[A] )

        def pack(v: A): DBObject = BasicDBObjectBuilder.start(name, v).get
    }

    case class nested[V](override val name: String, val element: DBObjectShape[V], val getter: Host => V)
            extends Field[Host, V, DBObject](name) {

        override val shape: DBObject = element.shape

        def apply(x: Host): V = getter(x)

        def extract(dbo: DBObject): Option[V] =
            ( tryo(dbo.get(name))
              filter {_.isInstanceOf[DBObject]}
              flatMap {element extract _.asInstanceOf[DBObject]} )

        def pack(v: V): DBObject = BasicDBObjectBuilder.start(name, element.pack(v)).get
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
}