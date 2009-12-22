package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex

sealed trait SortOrder {
    private[shape] def mongoOrder: Int
}

trait FieldCond[QueryType, A] { self: FieldIn =>
    import com.osinka.mongodb.Preamble.dotNotation
    import com.osinka.mongodb.wrapper._
    import MongoCondition._

    // Conditions
    def is_<(x: A) = QueryTerm[QueryType]( lt(mongoFieldPath, x) )

    def is_<=(x: A) = QueryTerm[QueryType]( le(mongoFieldPath, x) )

    def is_>(x: A) = QueryTerm[QueryType]( gt(mongoFieldPath, x) )

    def is_>=(x: A) = QueryTerm[QueryType]( ge(mongoFieldPath, x) )

    def is(x: A) = QueryTerm[QueryType]( eqTest(mongoFieldPath, x) )
    def is_==(x: A) = is(x)
    def eq_?(x: A) = is(x)
    def has(x: A) = is(x) // same for occurence in array

    def isNot(x: A) = QueryTerm[QueryType]( neTest(mongoFieldPath, x) )
    def not_==(x: A) = isNot(x)
    def ne_?(x: A) = not_==(x)

    def isIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.in(mongoFieldPath, x) )
    def in(x: List[A]) = isIn(x)

    def notIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.nin(mongoFieldPath, x) )
    def nin(x: List[A]) = notIn(x)

    def hasAll(x: List[A]) = QueryTerm[QueryType]( MongoCondition.all(mongoFieldPath, x) )
    def all(x: List[A]) = hasAll(x)

    def hasSize(x: Int) = QueryTerm[QueryType]( size(mongoFieldPath, x) )
    def ofSize(x: Int) = hasSize(x)

    def exists = QueryTerm[QueryType]( MongoCondition.exists(mongoFieldPath, true) )

    def notExists = QueryTerm[QueryType]( MongoCondition.exists(mongoFieldPath, false) )

    def like(x: Pattern) = QueryTerm[QueryType]( regex(mongoFieldPath,  x) )
    def is_~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[QueryType]( regex(mongoFieldPath,  x) )
    def is_~(x: Regex) = like(x)

    // Sorting
    case object Asc  extends SortOrder { override val mongoOrder = 1}
    case object Desc extends SortOrder { override val mongoOrder = -1 }

    def ascending  = this -> Asc
    def descending = this -> Desc
}