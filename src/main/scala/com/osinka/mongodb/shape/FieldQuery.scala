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

trait FieldCond[Host, QueryType, A] extends EmbeddableField { self: Field[Host, A, _] =>
    import MongoCondition._

    lazy val mongoFieldName = fieldPath.mkString(".")

    def is_<(x: A) = QueryTerm[QueryType]( lt(mongoFieldName, x) )
    def <(x: A) = is_<(x)

    def is_<=(x: A) = QueryTerm[QueryType]( le(mongoFieldName, x) )
    def <=(x: A) = is_<=(x)

    def is_>(x: A) = QueryTerm[QueryType]( gt(mongoFieldName, x) )
    def >(x: A) = is_>(x)

    def is_>=(x: A) = QueryTerm[QueryType]( ge(mongoFieldName, x) )
    def >=(x: A) = is_>=(x)

    def is_==(x: A) = QueryTerm[QueryType]( eqTest(mongoFieldName, x) )
    def ?==(x: A) = is_==(x)
    def eq_?(x: A) = is_==(x)
    def has(x: A) = is_==(x) // same for occurence in array

    def not_==(x: A) = QueryTerm[QueryType]( neTest(mongoFieldName, x) )
    def ?!=(x: A) = not_==(x)
    def ne_?(x: A) = not_==(x)

    def is_in(x: List[A]) = QueryTerm[QueryType]( MongoCondition.in(mongoFieldName, x) )
    def in(x: List[A]) = is_in(x)

    def not_in(x: List[A]) = QueryTerm[QueryType]( MongoCondition.nin(mongoFieldName, x) )
    def nin(x: List[A]) = not_in(x)

    def has_all(x: List[A]) = QueryTerm[QueryType]( MongoCondition.all(mongoFieldName, x) )
    def all(x: List[A]) = has_all(x)

    def has_size(x: Int) = QueryTerm[QueryType]( size(mongoFieldName, x) )
    def of_size(x: Int) = has_size(x)
    def has_#(x: Int) = has_size(x)

    def does_exist = QueryTerm[QueryType]( exists(mongoFieldName, true) )
    def exists_? = does_exist

    def not_exists = QueryTerm[QueryType]( exists(mongoFieldName, false) )

    def like(x: Pattern) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def ~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[QueryType]( regex(mongoFieldName,  x) )
    def ~(x: Regex) = like(x)
}