package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex

sealed trait SortOrder {
    private[shape] def mongoOrder: Int
}

trait FieldCond[Host, QueryType, A] { self: Field[Host, A] =>
    import com.osinka.mongodb.wrapper._
    import MongoCondition._

    private[shape] def fieldPath: List[String] = fieldName :: Nil

    private[shape] lazy val mongoFieldName = dotNotation(fieldPath)

    // Conditions
    def is_<(x: A) = QueryTerm[QueryType]( lt(mongoFieldName, x) )

    def is_<=(x: A) = QueryTerm[QueryType]( le(mongoFieldName, x) )

    def is_>(x: A) = QueryTerm[QueryType]( gt(mongoFieldName, x) )

    def is_>=(x: A) = QueryTerm[QueryType]( ge(mongoFieldName, x) )

    def is(x: A) = QueryTerm[QueryType]( eqTest(mongoFieldName, x) )
    def is_==(x: A) = is(x)
    def eq_?(x: A) = is(x)
    def has(x: A) = is(x) // same for occurence in array

    def isNot(x: A) = QueryTerm[QueryType]( neTest(mongoFieldName, x) )
    def not_==(x: A) = isNot(x)
    def ne_?(x: A) = not_==(x)

    def isIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.in(mongoFieldName, x) )
    def in(x: List[A]) = isIn(x)

    def notIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.nin(mongoFieldName, x) )
    def nin(x: List[A]) = notIn(x)

    def hasAll(x: List[A]) = QueryTerm[QueryType]( MongoCondition.all(mongoFieldName, x) )
    def all(x: List[A]) = hasAll(x)

    def hasSize(x: Int) = QueryTerm[QueryType]( size(mongoFieldName, x) )
    def ofSize(x: Int) = hasSize(x)

    def doesExist = QueryTerm[QueryType]( exists(mongoFieldName, true) )
    def exists_? = doesExist

    def notExists = QueryTerm[QueryType]( exists(mongoFieldName, false) )

    def like(x: Pattern) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def is_~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def is_~(x: Regex) = like(x)

    // Sorting
    case object Asc  extends SortOrder { override val mongoOrder = 1}
    case object Desc extends SortOrder { override val mongoOrder = -1 }

    def ascending  = this -> Asc
    def descending = this -> Desc
}