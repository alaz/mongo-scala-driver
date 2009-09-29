package com.osinka.mongodb.shape

import com.mongodb.{DBObject, BasicDBObjectBuilder}

trait BaseField[Host, A, FS] extends BaseShape[A, FS] with GetAndSet[Host, A] {
    def name: String
    def mongo_? : Boolean = name startsWith "$"
}

abstract case class Field[Host, A, FS](override val name: String) extends BaseField[Host, A, FS]

trait ShapeFields[Host] {

    abstract case class scalar[A](override val name: String) extends Field[Host, A, Int](name) {
        override val shape: Int = 1
    }

    /**
     * internal mongo field. always scalar
     */
    trait mongo[A] extends BaseField[Host, A, Int] { self: scalar[A] =>
        override def mongo_? = true
    }

    abstract case class nested[V, O <: DBObjectShape[V]](override val name: String, val element: DBObjectShape[V])
            extends Field[Host, V, DBObject](name) {

        override val shape: DBObject = element.shape

        override def getter(x: Host): Any = {
            val dbo = BasicDBObjectBuilder.start.get
            element.update(dbo, apply(x))
            dbo
        }

        override def setter(x: Host, v: Any): Unit = {
            update(x, element.apply(v.asInstanceOf[DBObject]))
        }
    }

    // TODO: ref
    // TODO: array

    // TODO: Bean-based field, Annotation-based shape, etc.
}