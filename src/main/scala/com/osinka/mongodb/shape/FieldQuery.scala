package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex

object MongoCondition {
    def cond(field: String, x: Any) = field -> x

    def op(op: String)(field: String, x: Any) = cond(field, Map(op -> x))

    def eqTest(field: String, x: Any) = cond(field, x)
    lazy val neTest = op("$ne") _

    lazy val lt = op("$lt") _
    lazy val le = op("$lte") _
    lazy val gt = op("$gt") _
    lazy val ge = op("$gte") _
    lazy val in = op("$in") _
    lazy val nin = op("$nin") _
    lazy val all = op("$all") _
//    def mod
    lazy val size = op("$size") _
    def exists(field: String, b: Boolean) = op("$exists")(field, b)

    def regex(field: String, x: Regex): (String, Any) = regex(field, x.pattern)

    def regex(field: String, x: Pattern): (String, Any) = eqTest(field, x)
}

sealed trait SortOrder {
    private[shape] def mongoOrder: Int
}

trait FieldCond[Host, QueryType, A] extends EmbeddableField { self: Field[Host, A, _] =>
    import MongoCondition._

    lazy val mongoFieldName = fieldPath.mkString(".")

    // Conditions
    def is_<(x: A) = QueryTerm[QueryType]( lt(mongoFieldName, x) )
    def <(x: A) = is_<(x)

    def is_<=(x: A) = QueryTerm[QueryType]( le(mongoFieldName, x) )
    def <=(x: A) = is_<=(x)

    def is_>(x: A) = QueryTerm[QueryType]( gt(mongoFieldName, x) )
    def >(x: A) = is_>(x)

    def is_>=(x: A) = QueryTerm[QueryType]( ge(mongoFieldName, x) )
    def >=(x: A) = is_>=(x)

    def is(x: A) = QueryTerm[QueryType]( eqTest(mongoFieldName, x) )
    def is_==(x: A) = is(x)
    def ?==(x: A) = is(x)
    def eq_?(x: A) = is(x)
    def has(x: A) = is(x) // same for occurence in array

    def isNot(x: A) = QueryTerm[QueryType]( neTest(mongoFieldName, x) )
    def not_==(x: A) = isNot(x)
    def ?!=(x: A) = not_==(x)
    def ne_?(x: A) = not_==(x)

    def isIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.in(mongoFieldName, x) )
    def in(x: List[A]) = isIn(x)

    def notIn(x: List[A]) = QueryTerm[QueryType]( MongoCondition.nin(mongoFieldName, x) )
    def nin(x: List[A]) = notIn(x)

    def hasAll(x: List[A]) = QueryTerm[QueryType]( MongoCondition.all(mongoFieldName, x) )
    def all(x: List[A]) = hasAll(x)

    def hasSize(x: Int) = QueryTerm[QueryType]( size(mongoFieldName, x) )
    def ofSize(x: Int) = hasSize(x)
    def has_#(x: Int) = hasSize(x)

    def doesExist = QueryTerm[QueryType]( exists(mongoFieldName, true) )
    def exists_? = doesExist

    def notExists = QueryTerm[QueryType]( exists(mongoFieldName, false) )

    def like(x: Pattern) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def ~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def ~(x: Regex) = like(x)

    // Sorting
    case object Asc  extends SortOrder { override val mongoOrder = 1}
    case object Desc extends SortOrder { override val mongoOrder = -1 }

    def ascending  = this -> Asc
    def descending = this -> Desc
}