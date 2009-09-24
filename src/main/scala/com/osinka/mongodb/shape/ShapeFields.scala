package com.osinka.mongodb.shape

import com.mongodb.{DBObject, BasicDBObjectBuilder}

abstract case class Field[Host, A, FS](val name: String)
        extends BaseShape[A, FS] with GetAndSet[Host, A] {
    def mongo_? : Boolean = name startsWith "$"
}

trait ShapeFields[Host, S] {

    abstract case class scalar[A](override val name: String) extends Field[Host, A, Int](name) {
        override val shape: Int = 1
    }

/*    case class fnField[A](override val name: String,
                          val get: Host => A,
                          val set: (Host, A) => Unit)
            extends scalar[A](name) {

        override def apply(x: Host): A = get(x)
        override def update(x: Host, v: A) = set(x, v)
    }*/

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