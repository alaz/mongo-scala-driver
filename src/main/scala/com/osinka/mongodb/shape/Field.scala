package com.osinka.mongodb.shape

import com.mongodb.{ObjectId, DBObject}
import com.osinka.mongodb._
import Preamble.{tryo, EmptyConstraints, pfToOption, dotNotation}
import wrapper.{DBO, MongoCondition}

trait FieldContainer {
    private[shape] def fieldPath: List[String] = Nil
}

trait FieldIn { self: ObjectField[_] =>
    private[shape] def fieldPath: List[String] = name :: Nil
    private[shape] lazy val mongoFieldName = dotNotation(fieldPath)
}

trait ShapeFields[T, QueryType] extends FieldContainer { parent =>
    /**
     * Mongo field can be of two kinds only: scalar and array
     */
    trait MongoField[A] extends ObjectField[T] with ObjectFieldWriter[T] { storage: FieldContent[A] =>
        val rep: FieldRep[_]
        override def constraints = rep postprocess storage.contentConstraints
    }

    trait MongoScalar[A] extends MongoField[A] { storage: FieldContent[A] =>
        override val rep: FieldRep[A]
        private[shape] def readFrom(x: T): Option[Any] = rep.get(x) flatMap storage.serialize
        private[shape] def writeTo(x: T, v: Option[Any]) { rep.put(x)(v flatMap storage.deserialize) }
    }

    trait MongoArray[A, C <: Seq[A]] extends MongoField[A] { storage: FieldContent[A] =>
        override val rep: FieldRep[C]
        private[shape] def readFrom(x: T): Option[Any] = rep.get(x) map { _ map storage.serialize }
        private[shape] def writeTo(x: T, v: Option[Any]) {
            /*
            rep.put(x)(v map {
                // TODO: how to get array out of DBObject, how it looks like inside?
                case dbo: DBObject => Seq.empty flatMap storage.deserialize
            })
            */
        }
    }

    /**
     * Field content: scalar, ref, embedded
     */
    trait FieldContent[A] { self: ObjectField[T] =>
        def existsConstraint(n: String): Map[String, Map[String, Boolean]] = Map( MongoCondition.exists(n, true) )

        def serialize(x: A): Option[Any]
        def deserialize(v: Any): Option[A]
        def contentConstraints: Map[String, Map[String, Boolean]]
    }

    trait ScalarContent[A] extends FieldContent[A] with FieldIn with FieldCond[QueryType, A] { self: MongoField[A] =>
        override val fieldPath = parent.fieldPath ::: super.fieldPath
        override def contentConstraints = existsConstraint(mongoFieldName)

        override def serialize(a: A) = Some(a)
        override def deserialize(v: Any) = Some(v.asInstanceOf[A])
    }
    
    trait RefContent[V <: MongoObject] extends FieldContent[V] with FieldIn { self: MongoField[V] =>
        val coll: MongoCollection[V]

        override def serialize(a: V) =
            if (a.mongoOID == null) None
            else Some(DBO.fromMap(Map(
                    "_ref" -> coll.getName,
                    "_id" -> a.mongoOID
                )))
        override def deserialize(v: Any) = v match {
            case dbo: DBObject if dbo.containsField("_id") =>
                dbo.get("_id") match {
                    case oid: ObjectId => pfToOption(coll)(oid)
                    case _ => None
                }
            case _ => None
        }
        override val fieldPath = parent.fieldPath ::: super.fieldPath
        override def contentConstraints = existsConstraint(mongoFieldName)
    }
    
    trait EmbeddedContent[V] extends FieldContent[V] with FieldContainer { objectShape: MongoField[V] with ObjectIn[V, QueryType] =>
        override val fieldPath = parent.fieldPath ::: name :: Nil
        override def contentConstraints: Map[String, Map[String, Boolean]] =
            (EmptyConstraints /: objectShape.constraints) { (m, e) =>
                m + (dotNotation(fieldPath ::: e._1 :: Nil) -> e._2)
            }

        override def serialize(a: V) = Some(objectShape.in(a))
        override def deserialize(v: Any) = v match {
            case dbo: DBObject => objectShape.out(dbo)
            case _ => None
        }
    }

    /**
     * Representation of a field in Scala objects
     */
    trait FieldRep[A] {
        def postprocess(constraints: Map[String, Map[String,Boolean]]): Map[String, Map[String,Boolean]] = constraints
        def get(x: T): Option[A]
        def put[B <: A](x: T)(a: Option[B])
    }

    /**
     * Helpers to ease life
     */

    def ordinary[A](field: MongoField[A], g: T => A, p: Option[(T, A) => Unit]) = new FieldRep[A] {
        override def get(x: T) = Some(g(x))
        override def put[B <: A](x: T)(a: Option[B]) {
            for {func <- p; value <- a} func(x, value)
        }
    }

    def optional[A](field: MongoField[A], g: T => Option[A], p: Option[(T, Option[A]) => Unit]) = new FieldRep[A] {
        override def postprocess(constraints: Map[String, Map[String,Boolean]]) = Map.empty[String, Map[String,Boolean]]
        override def get(x: T) = g(x)
        override def put[B <: A](x: T)(a: Option[B]) {
            for {func <- p} func(x, a)
        }
    }

    /**
     * Scalar field
     */
    class Scalar[A](override val name: String, val g: T => A, val p: Option[(T,A) => Unit]) extends MongoScalar[A] with ScalarContent[A] {
        def this(n: String, g: T => A, p: (T,A) => Unit) {
            this(n, g, Some(p))
        }

        def this(n: String, g: T => A) {
            this(n, g, None)
        }

        override val rep = ordinary(this, g, p)
    }

    /**
     * Shape object living in a field.
     *
     * For instantiation as an object: ObjectIn should be mixed in.
     */
    class Embedded[V](override val name: String, val g: T => V, val p: Option[(T,V) => Unit]) extends MongoScalar[V] with EmbeddedContent[V] {
        self: MongoField[V] with ObjectIn[V, QueryType] =>
        
        override val rep = parent.ordinary(this, g, p)
    }

    /**
     * Scalar field factory
     */
    object Scalar {
        def apply[A](fieldName: String, getter: T => A) = new Scalar[A](fieldName, getter) with Functional[A]

        def apply[A](fieldName: String, getter: T => A, setter: (T, A) => Unit) =
            new Scalar[A](fieldName, getter, Some(setter)) with Functional[A]
    }

    /*
     * Optional field
     */
    class Optional[A](override val name: String, val g: T => Option[A], val p: Option[(T,Option[A]) => Unit]) extends MongoScalar[A] with ScalarContent[A] {
        override def constraints = EmptyConstraints

        def this(n: String, g: T => Option[A], p: (T,Option[A]) => Unit) {
            this(n, g, Some(p))
        }

        def this(n: String, g: T => Option[A]) {
            this(n, g, None)
        }

        override val rep = optional(this, g, p)
    }

    /**
     * Optional scalar field factory
     */
    object Optional {
        def apply[A](fieldName: String, getter: T => Option[A]) = new Optional[A](fieldName, getter) with Functional[A]

        def apply[A](fieldName: String, getter: T => Option[A], setter: (T, Option[A]) => Unit) =
            new Optional[A](fieldName, getter, setter) with Functional[A]
    }

    /**
     * Support for unapplying parent's DBO
     */
    trait Functional[A] { self: MongoScalar[A] with FieldContent[A] =>
        def unapply(dbo: DBObject): Option[A] = tryo(dbo get name) flatMap self.deserialize
    }
}