/**
 * Copyright (C) 2009-2010 Alexander Azarov <azarov@osinka.ru>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.osinka.mongodb.shape

import com.mongodb.{ObjectId, DBObject}
import com.osinka.mongodb._
import Preamble.{tryo, pfToOption, dotNotation}
import wrapper.DBO

trait ObjectField {
    def mongoFieldName: String
    private[shape] def mongoFieldPath: List[String] = List(mongoFieldName)
    lazy val longFieldName = dotNotation(mongoFieldPath)
}

trait FieldContainer {
    private[shape] def containerPath: List[String] = Nil
}

trait ShapeFields[T, QueryType] extends FieldContainer
        with FieldQueryConditions[T, QueryType] with FieldModifyOperations[T, QueryType] { parent =>

    /**
     * Mongo field can be of two kinds only: scalar and array
     */
    trait MongoField[A] extends ObjectField with FieldConditions[A] { storage: FieldContent[A] =>
        def mongoInternal_? : Boolean = mongoFieldName startsWith "_"
        def mongoConstraints: QueryTerm[QueryType] = rep postprocess storage.contentConstraints

        private[shape] def mongoReadFrom(x: T): Option[Any]
        private[shape] def mongoWriteTo(x: T, v: Option[Any])

        protected def rep: FieldRep[_]
        override def mongoFieldPath: List[String] = parent.containerPath ::: super.mongoFieldPath
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

        // An array constraints only array field existance and nothing more.
        // It could set a constraint on the array's values, but array can be empty
        override def mongoConstraints = exists

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
    trait FieldContent[A] { self: ObjectField =>

        protected def serialize(x: A): Option[Any]
        protected def deserialize(v: Any): Option[A]
        protected def contentConstraints: QueryTerm[QueryType]
    }

    trait ScalarContent[A] extends FieldContent[A] with ScalarContentConditions[A] { self: MongoField[A] =>
        override def contentConstraints = exists

        override def serialize(a: A) = Some(a)
        override def deserialize(v: Any) = Some(v.asInstanceOf[A])
    }
    
    trait RefContent[V <: MongoObject] extends FieldContent[V] with RefContentConditions[V] { self: MongoField[V] =>
        protected val coll: MongoCollection[V]

        override def serialize(a: V) = a.mongoOID map {oid =>
            DBO.fromMap(Map(
                "_ref" -> coll.getName,
                "_id" -> oid
            ))
        }
        override def deserialize(v: Any) = v match {
            case dbo: DBObject if dbo.containsField("_id") =>
                dbo.get("_id") match {
                    case oid: ObjectId => pfToOption(coll)(oid)
                    case _ => None
                }
            case _ => None
        }
        override def contentConstraints = exists
    }
    
    trait EmbeddedContent[V] extends FieldContent[V] with FieldContainer { objectShape: MongoField[V] with ObjectIn[V, QueryType] =>
        override def containerPath = mongoFieldPath
        override def contentConstraints = objectShape.constraints

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
        def postprocess(constraints: QueryTerm[QueryType]) = constraints
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
            override def postprocess(constraints: QueryTerm[QueryType]) = QueryTerm[QueryType]()
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
            extends MongoScalar[A] with ScalarContent[A] with ScalarFieldModifyOp[A] {
        override val rep = Represented.by(g, p)
    }

    /*
     * Optional field
     */
    class OptionalField[A](override val mongoFieldName: String, val g: T => Option[A], val p: Option[(T,Option[A]) => Unit])
            extends MongoScalar[A] with ScalarContent[A] with ScalarFieldModifyOp[A] with Optional[A] {
        override val rep = Represented.byOption(g, p)
    }

    /**
     * Shape object living in a field.
     *
     * For instantiation as an object: ObjectIn should be mixed in.
     */
    class EmbeddedField[V](override val mongoFieldName: String, val g: T => V, val p: Option[(T,V) => Unit])
            extends MongoScalar[V] with EmbeddedContent[V] with FieldModifyOp[V] {
        self: MongoField[V] with ObjectIn[V, QueryType] =>
        
        override val rep = parent.Represented.by(g, p)
    }

    /**
     * Reference
     */
    class RefField[V <: MongoObject](override val mongoFieldName: String, override val coll: MongoCollection[V],
                                     val g: T => V, val p: Option[(T,V) => Unit])
            extends MongoScalar[V] with RefContent[V] {

        override val rep = parent.Represented.by(g, p)
    }

    /**
     * Optional reference
     */
    class OptionalRefField[V <: MongoObject](override val mongoFieldName: String, override val coll: MongoCollection[V],
                                             val g: T => Option[V], val p: Option[(T,Option[V]) => Unit])
            extends MongoScalar[V] with RefContent[V] with Optional[V] {

        override val rep = parent.Represented.byOption(g, p)
    }

    /**
     * Array of scalars
     */
    class ArrayField[A](override val mongoFieldName: String, val g: T => Seq[A], val p: Option[(T,Seq[A]) => Unit])
            extends MongoArray[A] with ScalarContent[A] with ArrayFieldModifyOp[A] {

        override val rep = Represented.by[Seq[A]](g, p)
    }

    /**
     * Array of embedded objects. Must be subclassed: ObjectIn should be mixed in.
     */
    class ArrayEmbeddedField[V](override val mongoFieldName: String, val g: T => Seq[V], val p: Option[(T,Seq[V]) => Unit])
            extends MongoArray[V] with EmbeddedContent[V] with ArrayFieldModifyOp[V] {
        self: MongoField[V] with ObjectIn[V, QueryType] =>

        override val rep = parent.Represented.by(g, p)
    }

    /**
     * Array of references
     */
    class ArrayRefField[V <: MongoObject](override val mongoFieldName: String, override val coll: MongoCollection[V],
                                          val g: T => Seq[V], val p: Option[(T,Seq[V]) => Unit])
            extends MongoArray[V] with RefContent[V] {

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

        def ref[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => V) =
            new RefField[V](fieldName, coll, getter, None) with Functional[V]

        def ref[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => V, setter: (T, V) => Unit) =
            new RefField[V](fieldName, coll, getter, Some(setter)) with Functional[V]

        def optionalRef[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => Option[V]) =
            new OptionalRefField[V](fieldName, coll, getter, None) with Functional[V]
        
        def optionalRef[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => Option[V], setter: (T, Option[V]) => Unit) =
            new OptionalRefField[V](fieldName, coll, getter, Some(setter)) with Functional[V]

        def array[A](fieldName: String, getter: T => Seq[A]) =
            new ArrayField[A](fieldName, getter, None)

        def array[A](fieldName: String, getter: T => Seq[A], setter: (T, Seq[A]) => Unit) =
            new ArrayField[A](fieldName, getter, Some(setter))

        def arrayRef[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => Seq[V]) =
            new ArrayRefField[V](fieldName, coll, getter, None)

        def arrayRef[V <: MongoObject](fieldName: String, coll: MongoCollection[V], getter: T => Seq[V], setter: (T, Seq[V]) => Unit) =
            new ArrayRefField[V](fieldName, coll, getter, Some(setter))
    }

    /**
     * Optional field means it imposes no constraint
     */
    trait Optional[A] extends MongoField[A] { self: FieldContent[A] =>
        override def mongoConstraints = QueryTerm[QueryType]()
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