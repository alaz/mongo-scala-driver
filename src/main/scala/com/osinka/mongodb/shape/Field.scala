package com.osinka.mongodb.shape

import com.mongodb.{ObjectId, DBObject}
import com.osinka.mongodb._
import Preamble.{tryo, EmptyConstraints, pfToOption, dotNotation}
import wrapper.{DBO, MongoCondition}

trait FieldContainer {
    private[shape] def containerPath: List[String] = Nil
}

trait FieldIn { self: ObjectField[_] =>
    private[shape] def mongoFieldPath: List[String] = mongoFieldName :: Nil
}

trait ShapeFields[T, QueryType] extends FieldContainer { parent =>
    /**
     * Mongo field can be of two kinds only: scalar and array
     */
    trait MongoField[A] extends ObjectField[T] with ObjectFieldWriter[T] { storage: FieldContent[A] =>
        protected def rep: FieldRep[_]
        override def mongoConstraints = rep postprocess storage.contentConstraints
    }

    trait MongoScalar[A] extends MongoField[A] { storage: FieldContent[A] =>
        override def rep: FieldRep[A]

        private[shape] def mongoReadFrom(x: T): Option[Any] =
            rep.get(x) flatMap storage.serialize

        private[shape] def mongoWriteTo(x: T, v: Option[Any]) {
            rep.put(x)(v flatMap storage.deserialize)
        }
    }

    trait MongoArray[A] extends MongoField[A] { storage: FieldContent[A] =>
        override def rep: FieldRep[Seq[A]]

        private[shape] def mongoReadFrom(x: T): Option[Any] =
            rep.get(x) map { _ map storage.serialize }

        private[shape] def mongoWriteTo(x: T, v: Option[Any]) {
            rep.put(x)(v map {
                case dbo: DBObject =>
                    DBO.toArray(dbo).flatMap{Preamble.tryo[Any]}.flatMap{storage.deserialize}
            })
        }
    }

    /**
     * Field content: scalar, ref, embedded
     */
    trait FieldContent[A] { self: ObjectField[T] =>
        protected def existsConstraint(n: String): Map[String, Map[String, Boolean]] = Map( MongoCondition.exists(n, true) )

        protected def serialize(x: A): Option[Any]
        protected def deserialize(v: Any): Option[A]
        protected def contentConstraints: Map[String, Map[String, Boolean]]
    }

    trait ScalarContent[A] extends FieldContent[A] with FieldIn with FieldCond[QueryType, A] { self: MongoField[A] =>
        override def mongoFieldPath = parent.containerPath ::: super.mongoFieldPath
        override def contentConstraints = existsConstraint(dotNotation(mongoFieldPath))

        override def serialize(a: A) = Some(a)
        override def deserialize(v: Any) = Some(v.asInstanceOf[A])
    }
    
    trait RefContent[V <: MongoObject] extends FieldContent[V] with FieldIn { self: MongoField[V] =>
        protected val coll: MongoCollection[V]

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
        override def mongoFieldPath = parent.containerPath ::: super.mongoFieldPath
        override def contentConstraints = existsConstraint(dotNotation(mongoFieldPath))
    }
    
    trait EmbeddedContent[V] extends FieldContent[V] with FieldContainer { objectShape: MongoField[V] with ObjectIn[V, QueryType] =>
        override def containerPath = parent.containerPath ::: mongoFieldName :: Nil
        override def contentConstraints: Map[String, Map[String, Boolean]] = objectShape.constraints

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
        def get[A1>:A](x: T): Option[A1]
        def put[A2<:A](x: T)(a: Option[A2])
    }

    /**
     * Helpers to ease life
     */
    object Represented {
        def by[A](g: T => A, p: Option[(T, A) => Unit]) = new FieldRep[A] {
            override def get[A1>:A](x: T): Option[A1] = Some(g(x))
            override def put[A2<:A](x: T)(a: Option[A2]) {
                for {func <- p; value <- a} func(x, value)
            }
        }

        def byOption[A](g: T => Option[A], p: Option[(T, Option[A]) => Unit]) = new FieldRep[A] {
            override def postprocess(constraints: Map[String, Map[String,Boolean]]) = EmptyConstraints
            override def get[A1>:A](x: T): Option[A1] = g(x)
            override def put[A2<:A](x: T)(a: Option[A2]) {
                for {func <- p} func(x, a)
            }
        }
    }

    /**
     * Scalar field
     */
    class ScalarField[A](override val mongoFieldName: String, val g: T => A, val p: Option[(T,A) => Unit])
            extends MongoScalar[A] with ScalarContent[A] {
        override val rep = Represented.by(g, p)
    }

    /*
     * Optional field
     */
    class OptionalField[A](override val mongoFieldName: String, val g: T => Option[A], val p: Option[(T,Option[A]) => Unit])
            extends MongoScalar[A] with ScalarContent[A] with Optional[A] {
        override val rep = Represented.byOption(g, p)
    }

    /**
     * Shape object living in a field.
     *
     * For instantiation as an object: ObjectIn should be mixed in.
     */
    class EmbeddedField[V](override val mongoFieldName: String, val g: T => V, val p: Option[(T,V) => Unit])
            extends MongoScalar[V] with EmbeddedContent[V] {
        self: MongoField[V] with ObjectIn[V, QueryType] =>
        
        override val rep = parent.Represented.by(g, p)
    }

    /**
     * Field factories
     */
    object Field {
        def scalar[A](fieldName: String, getter: T => A) =
            new ScalarField[A](fieldName, getter, None) with Functional[A]

        def scalar[A](fieldName: String, getter: T => A, setter: (T, A) => Unit) =
            new ScalarField[A](fieldName, getter, Some(setter)) with Functional[A]

        def optional[A](fieldName: String, getter: T => Option[A]) =
            new OptionalField[A](fieldName, getter, None) with Functional[A]

        def optional[A](fieldName: String, getter: T => Option[A], setter: (T, Option[A]) => Unit) =
            new OptionalField[A](fieldName, getter, Some(setter)) with Functional[A]
    }

    /**
     * Optional field means it imposes no constraint
     */
    trait Optional[A] extends MongoField[A] { self: FieldContent[A] =>
        override def mongoConstraints = EmptyConstraints
    }

    /**
     * Some useful extra methods for fields, like
     *   dbo match { case field(value) => ... }
     *
     * or in case of mandatory constructor argument
     *   for {field(v) <- Some(dbo)} yield new Obj(...., v, ...)
     *
     * or in case of optional field
     *   new Obj(..., field from dbo, ...)
     */
    trait Functional[A] { self: MongoScalar[A] with FieldContent[A] =>
        def unapply(dbo: DBObject): Option[A] = tryo(dbo get mongoFieldName) flatMap self.deserialize
        def from(dbo: DBObject) = unapply(dbo)
    }
}