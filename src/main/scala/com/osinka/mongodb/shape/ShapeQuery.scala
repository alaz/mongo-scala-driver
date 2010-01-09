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

import Preamble._

trait Queriable[T] { self: ObjectShape[T] =>
    type SortableFieldType = FieldInHierarchy with ScalarContentConditions[_]

    def where(query: QueryTerm[T]) = ShapeQuery() where query
    def drop(n: Int) = ShapeQuery() drop n
    def take(n: Int) = ShapeQuery() take n
    def sortBy(sorting: (SortableFieldType, SortOrder)*) = ShapeQuery().sortBy(sorting:_*)

    case class ShapeQuery(val filters: QueryTerm[T], val sortBy: List[(SortableFieldType, SortOrder)], private val q: Query) {
        def where(filter: QueryTerm[T]) = ShapeQuery(filters and filter, sortBy, q)

        def drop(n: Int): ShapeQuery = drop(Some(n))
        def drop(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q drop n)

        def take(n: Int): ShapeQuery = take(Some(n))
        def take(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q take n)

        def noSort = ShapeQuery(filters, sortBy, q sort None)
        def sortBy(s: (SortableFieldType, SortOrder)*): ShapeQuery = ShapeQuery(filters, s.toList ::: sortBy, q)

        def query: Query = {
            val s = (Map.empty[String, Int] /: sortBy) { (m, x) =>
                m + (x._1.longFieldName -> x._2.mongoOrder)
            }
            q ++ filters.m sort s
        }
    }

    object ShapeQuery {
        def apply() = new ShapeQuery(QueryTerm[T], Nil, Query())
        def apply(qt: QueryTerm[T]) = new ShapeQuery(qt, Nil, Query())
    }

    // TODO: Monadic query? http://github.com/alaz/mongo-scala-driver/issues#issue/13
}

sealed case class QueryTerm[+T](val m: Map[String, Any]) {
    def query = Query() ++ m

    def and[B >: T](q: QueryTerm[B]) = new QueryTerm[T](m ++ q.m)
}

object QueryTerm {
    def apply[T]() = new QueryTerm[T](Map.empty[String, Any])
    def apply[T](tuple: (String, Any)) = new QueryTerm[T](Map(tuple))
}