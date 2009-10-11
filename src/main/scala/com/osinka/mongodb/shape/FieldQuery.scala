package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex

object MongoCondition {
    def op(op: String)(name: String, x: Any) = name -> Map(op -> x)

    def eqTest(name: String, x: Any) = name -> x
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
    def exists(name: String, b: Boolean) = op("$exists")(name, b)

    def regex(name: String, x: Regex): (String, Any) = regex(name, x.pattern)

    def regex(name: String, x: Pattern): (String, Any) = name -> x
}

trait FieldCond[Host, A] { self: Field[Host, A, _] =>
    import MongoCondition._

    def is_<(x: A) = QueryTerm[Host]( lt(name, x) )
    def <(x: A) = is_<(x)

    def is_<=(x: A) = QueryTerm[Host]( le(name, x) )
    def <=(x: A) = is_<=(x)

    def is_>(x: A) = QueryTerm[Host]( gt(name, x) )
    def >(x: A) = is_>(x)

    def is_>=(x: A) = QueryTerm[Host]( ge(name, x) )
    def >=(x: A) = is_>=(x)

    def is_==(x: A) = QueryTerm[Host]( eqTest(name, x) )
    def ?==(x: A) = is_==(x)
    def eq_?(x: A) = is_==(x)
    def has(x: A) = is_==(x) // same for occurence in array

    def not_==(x: A) = QueryTerm[Host]( neTest(name, x) )
    def ?!=(x: A) = not_==(x)
    def ne_?(x: A) = not_==(x)

    def is_in(x: Seq[A]) = QueryTerm[Host]( MongoCondition.in(name, x) )
    def in(x: Seq[A]) = is_in(x)

    def not_in(x: Seq[A]) = QueryTerm[Host]( MongoCondition.nin(name, x) )
    def nin(x: Seq[A]) = not_in(x)

    def has_all(x: Seq[A]) = QueryTerm[Host]( MongoCondition.all(name, x) )
    def all(x: Seq[A]) = has_all(x)

    def has_size(x: Int) = QueryTerm[Host]( size(name, x) )
    def of_size(x: Int) = has_size(x)
    def has_#(x: Int) = has_size(x)

    def does_exist = QueryTerm[Host]( exists(name, true) )
    def exists_? = does_exist

    def not_exists = QueryTerm[Host]( exists(name, false) )

    def like(x: Pattern) = QueryTerm[Host]( regex(name,  x) )
    def ~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[Host]( regex(name,  x) )
    def ~(x: Regex) = like(x)
}