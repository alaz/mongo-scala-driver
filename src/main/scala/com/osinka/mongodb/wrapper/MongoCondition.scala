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