package com.osinka.mongodb.shape

import java.util.regex.Pattern
import scala.util.matching.Regex

object MongoCondition {
    def cond(nlist: List[String], x: Any) = nlist.mkString(".") -> x

    def op(op: String)(nlist: List[String], x: Any) = cond(nlist, Map(op -> x))

    def eqTest(nlist: List[String], x: Any) = cond(nlist, x)
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
    def exists(nlist: List[String], b: Boolean) = op("$exists")(nlist, b)

    def regex(nlist: List[String], x: Regex): (String, Any) = regex(nlist, x.pattern)

    def regex(nlist: List[String], x: Pattern): (String, Any) = eqTest(nlist, x)
}

trait FieldCond[Host, A] { self: Field[Host, A, _] =>
    import MongoCondition._

    def prefix: List[String]

    def is_<(x: A) = QueryTerm[Host]( lt(name :: prefix, x) )
    def <(x: A) = is_<(x)

    def is_<=(x: A) = QueryTerm[Host]( le(name :: prefix, x) )
    def <=(x: A) = is_<=(x)

    def is_>(x: A) = QueryTerm[Host]( gt(name :: prefix, x) )
    def >(x: A) = is_>(x)

    def is_>=(x: A) = QueryTerm[Host]( ge(name :: prefix, x) )
    def >=(x: A) = is_>=(x)

    def is_==(x: A) = QueryTerm[Host]( eqTest(name :: prefix, x) )
    def ?==(x: A) = is_==(x)
    def eq_?(x: A) = is_==(x)
    def has(x: A) = is_==(x) // same for occurence in array

    def not_==(x: A) = QueryTerm[Host]( neTest(name :: prefix, x) )
    def ?!=(x: A) = not_==(x)
    def ne_?(x: A) = not_==(x)

    def is_in(x: List[A]) = QueryTerm[Host]( MongoCondition.in(name :: prefix, x) )
    def in(x: List[A]) = is_in(x)

    def not_in(x: List[A]) = QueryTerm[Host]( MongoCondition.nin(name :: prefix, x) )
    def nin(x: List[A]) = not_in(x)

    def has_all(x: List[A]) = QueryTerm[Host]( MongoCondition.all(name :: prefix, x) )
    def all(x: List[A]) = has_all(x)

    def has_size(x: Int) = QueryTerm[Host]( size(name :: prefix, x) )
    def of_size(x: Int) = has_size(x)
    def has_#(x: Int) = has_size(x)

    def does_exist = QueryTerm[Host]( exists(name :: prefix, true) )
    def exists_? = does_exist

    def not_exists = QueryTerm[Host]( exists(name :: prefix, false) )

    def like(x: Pattern) = QueryTerm[Host]( regex(name :: prefix,  x) )
    def ~(x: Pattern) = like(x)

    def like(x: Regex) = QueryTerm[Host]( regex(name :: prefix,  x) )
    def ~(x: Regex) = like(x)
}