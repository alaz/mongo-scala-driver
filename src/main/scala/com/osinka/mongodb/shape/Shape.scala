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

import scala.reflect.Manifest
import com.mongodb.{DBObject, DBCollection}
import Preamble.{tryo, EmptyConstraints}
import wrapper.DBO

trait ObjectFieldReader[T] {
    private[shape] def mongoReadFrom(x: T): Option[Any]
}

trait ObjectField[T] extends ObjectFieldReader[T] {
    def mongoFieldName: String
    def mongoInternal_? : Boolean = mongoFieldName startsWith "_"
    def mongoConstraints: Map[String, Map[String, Boolean]]
}

trait ObjectFieldWriter[T] { self: ObjectField[T] =>
    private[shape] def mongoWriteTo(x: T, v: Option[Any])
}

/*
 * Shape of an object held in some other object (being it a Shape or Query)
 */
trait ObjectIn[T, QueryType] extends Serializer[T] with ShapeFields[T, QueryType] {
    def * : List[ObjectField[T]]
    def factory(dbo: DBObject): Option[T]

    protected def fieldList: List[ObjectField[T]] = *

    lazy val constraints = (fieldList remove {_.mongoInternal_?} foldLeft EmptyConstraints) { (m,f) =>
        assert(f != null, "Field must not be null")
        m ++ f.mongoConstraints
    }

    private[shape] def packFields(x: T, fields: Seq[ObjectField[T]]): DBObject =
        DBO.fromMap( (fields foldLeft Map[String,Any]() ) { (m,f) =>
            assert(f != null, "Field must not be null")
            f.mongoReadFrom(x) match {
                case Some(v) => m + (f.mongoFieldName -> v)
                case None => m
            }
        } )

    private[shape] def updateFields(x: T, dbo: DBObject, fields: Seq[ObjectField[T]]) {
        for {f <- fields if f.isInstanceOf[ObjectFieldWriter[_]]
             updatableField = f.asInstanceOf[ObjectFieldWriter[T]] }
            updatableField.mongoWriteTo(x, tryo(dbo get f.mongoFieldName))
    }

    // -- Serializer[T]
    override def in(x: T): DBObject = packFields(x, fieldList)

    override def out(dbo: DBObject) = factory(dbo) map { x =>
        assert(x != null, "Factory should not return Some(null)")
        updateFields(x, dbo, fieldList)
        x
    }

    override def mirror(x: T)(dbo: DBObject) = {
        assert(x != null, "Mirror should not be called on null")
        updateFields(x, dbo, fieldList filter { _.mongoInternal_? })
        x
    }
}

/*
 * Shape of an object backed by DBObject ("hosted in")
 */
trait ObjectShape[T] extends ObjectIn[T, T] with Queriable[T] {
    def collection(underlying: DBCollection) = new ShapedCollection[T](underlying, this)
}

/**
 * Mix-in to make a shape functional
 *
 * FunctionalShape makes a shape with convinient syntactic sugar
 * for converting object to DBObject (apply) and extractor for the opposite
 *
 * E.g.
 * val dbo = UserShape(u)
 * dbo match {
 *    case UserShape(u) =>
 * }
 */
trait FunctionalShape[T] { self: ObjectShape[T] =>
    def apply(x: T): DBObject = in(x)
    def unapply(rep: DBObject): Option[T] = out(rep)
}

/**
 * Shape of MongoObject child.
 *
 * It has mandatory _id and _ns fields
 */
trait MongoObjectShape[T <: MongoObject] extends ObjectShape[T] {
    import com.mongodb.ObjectId

    lazy val oid = Field.optional("_id", (x: T) => x.mongoOID, (x: T, oid: Option[ObjectId]) => x.mongoOID = oid)
    lazy val ns = Field.optional("_ns", (x: T) => x.mongoNS, (x: T, ns: Option[String]) => x.mongoNS = ns)

//    object oid extends ScalarField[ObjectId]("_id", (x: T) => x.mongoOID, Some( (x: T, oid: ObjectId) => x.mongoOID = oid) )
//            with Functional[ObjectId] with Optional[ObjectId]
//
//    object ns extends ScalarField[String]("_ns", (x: T) => x.mongoNS, Some( (x: T, ns: String) => x.mongoNS = ns) )
//            with Functional[String] with Optional[String]

    // -- ObjectShape[T]
    override def fieldList : List[ObjectField[T]] = oid :: ns :: super.fieldList
}