package com.osinka.mongodb.wrapper

import java.util.regex.Pattern
import scala.util.matching.Regex

object MongoCondition {
    def cond[T](field: String, x: T) = field -> x

    def op[T](op: String)(field: String, x: T) = cond(field, Map(op -> x))

    def eqTest[T](field: String, x: T) = cond(field, x)
    lazy val neTest = op[Any]("$ne") _

    lazy val lt = op[Any]("$lt") _
    lazy val le = op[Any]("$lte") _
    lazy val gt = op[Any]("$gt") _
    lazy val ge = op[Any]("$gte") _
    lazy val in = op[Any]("$in") _
    lazy val nin = op[Any]("$nin") _
    lazy val all = op[Any]("$all") _
//    def mod
    lazy val size = op[Any]("$size") _
    def exists(field: String, b: Boolean) = op("$exists")(field, b)

    def regex(field: String, x: Regex): (String, Pattern) = regex(field, x.pattern)
    def regex(field: String, x: Pattern): (String, Pattern) = eqTest(field, x)
}