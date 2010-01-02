package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex
import com.osinka.mongodb._
import Preamble.dotNotation
import wrapper._

object Constraints {
    import com.osinka.mongodb.wrapper.MongoCondition

    def existsConstraint(name: String): Map[String, Map[String, Boolean]] = Map( MongoCondition.exists(name, true) )
}

sealed trait SortOrder {
    private[shape] def mongoOrder: Int
}

trait FieldCond[QueryType, A] { self: FieldInHierarchy =>
    import MongoCondition._

    // Conditions
    def is_<(x: A) = QueryTerm[QueryType]( lt(longFieldName, x) )

    def is_<=(x: A) = QueryTerm[QueryType]( le(longFieldName, x) )

    def is_>(x: A) = QueryTerm[QueryType]( gt(longFieldName, x) )

    def is_>=(x: A) = QueryTerm[QueryType]( ge(longFieldName, x) )

    def is(x: A) = QueryTerm[QueryType]( eqTest(longFieldName, x) )
    def is_==(x: A) = is(x)
    def eq_?(x: A) = is(x)
    def has(x: A) = is(x) // same for occurence in array

    def isNot(x: A) = QueryTerm[QueryType]( neTest(longFieldName, x) )
    def not_==(x: A) = isNot(x)
    def ne_?(x: A) = not_==(x)

    def isIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.in(longFieldName, x) )
    def in(x: List[A]) = isIn(x)

    def notIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.nin(longFieldName, x) )
    def nin(x: List[A]) = notIn(x)

    def hasAll(x: List[A]) = QueryTerm[QueryType]( MongoCondition.all(longFieldName, x) )
    def all(x: List[A]) = hasAll(x)

    def hasSize(x: Int) = QueryTerm[QueryType]( size(longFieldName, x) )
    def ofSize(x: Int) = hasSize(x)

    def exists = QueryTerm[QueryType]( MongoCondition.exists(longFieldName, true) )

    def notExists = QueryTerm[QueryType]( MongoCondition.exists(longFieldName, false) )

    def like(x: Pattern) = QueryTerm[QueryType]( regex(longFieldName,  x) )
    def is_~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[QueryType]( regex(longFieldName,  x) )
    def is_~(x: Regex) = like(x)

    // Sorting
    case object Asc  extends SortOrder { override val mongoOrder = 1}
    case object Desc extends SortOrder { override val mongoOrder = -1 }

    def ascending  = this -> Asc
    def descending = this -> Desc
}

trait RefCond[QueryType, V <: MongoObject] { self: FieldInHierarchy =>
    import com.osinka.mongodb.Preamble.dotNotation
    import com.osinka.mongodb.wrapper._
    import MongoCondition._

    // Conditions
    def is(x: V) = x.mongoOID map { oid =>
            val fieldId = longFieldName+"._id"
            QueryTerm[QueryType]( eqTest(fieldId, oid) )
        }  getOrElse notExists
    def is_==(x: V) = is(x)
    def eq_?(x: V) = is(x)
    def has(x: V) = is(x) // same for occurence in array

    def isNot(x: V) = x.mongoOID map { oid =>
            val fieldId = longFieldName+"._id"
            QueryTerm[QueryType]( neTest(fieldId, oid) )
        } getOrElse exists
    def not_==(x: V) = isNot(x)
    def ne_?(x: V) = not_==(x)

    def isIn(x: List[V]) = {
        val fieldId = longFieldName+"._id"
        QueryTerm[QueryType]( MongoCondition.in(fieldId, x flatMap {_.mongoOID}) )
    }
    def in(x: List[V]) = isIn(x)

    def notIn(x: List[V]) = {
        val fieldId = longFieldName+"._id"
        QueryTerm[QueryType]( MongoCondition.nin(fieldId, x flatMap {_.mongoOID}) )
    }
    def nin(x: List[V]) = notIn(x)

    def exists = QueryTerm[QueryType]( MongoCondition.exists(longFieldName, true) )

    def notExists = QueryTerm[QueryType]( MongoCondition.exists(longFieldName, false) )
}