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

import com.osinka.mongodb._

/**
 * Query factory for ObjectShape
 */
trait Queriable[T] { self: ObjectShape[T] =>
    type SortableFieldType = ObjectField with ScalarContentConditions[_]

    /**
     * Empty query: any document qualifies
     */
    def any = ShapeQuery()

    /**
     * Query based on field conditions
     */
    def where(query: QueryTerm[T]): ShapeQuery = ShapeQuery() where query

    /**
     * Query that drops n first documents
     */
    def drop(n: Int) = ShapeQuery() drop n

    /**
     * Query that limits the resulting collection by n documents
     */
    def take(n: Int) = ShapeQuery() take n

    /**
     * Query where results are sorted
     */
    def sortBy(sorting: (SortableFieldType, SortOrder)*) = ShapeQuery().sortBy(sorting:_*)

    /**
     * Immutable query to apply to ShapedCollection
     */
    sealed case class ShapeQuery(val filters: QueryTerm[T], val sortBy: List[(SortableFieldType, SortOrder)], private val q: Query) {
        /**
         * Apply the query to collection
         */
        def in[Coll <: QueriedCollection[T, Coll]](coll: Coll): Coll = coll.applied(query)

        def where(filter: QueryTerm[T]): ShapeQuery = ShapeQuery(filters and filter, sortBy, q)

        def drop(n: Int): ShapeQuery = drop(Some(n))
        def drop(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q drop n)

        def take(n: Int): ShapeQuery = take(Some(n))
        def take(n: Option[Int]): ShapeQuery = ShapeQuery(filters, sortBy, q take n)

        /**
         * modified query, no sorting
         */
        def noSort = ShapeQuery(filters, sortBy, q sort None)
        def sortBy(s: (SortableFieldType, SortOrder)*): ShapeQuery = ShapeQuery(filters, s.toList ::: sortBy, q)

        def query: Query = {
            val s = (Map.empty[String, Int] /: sortBy) { (m, x) =>
                m + (x._1.longFieldName -> x._2.mongoOrder)
            }
            q ++ filters.m sort s
        }
    }

    /**
     * Factory of queries
     */
    object ShapeQuery {
        /**
         * Empty query
         */
        def apply() = new ShapeQuery(QueryTerm[T], Nil, Query())

        /**
         * Query based on field conditions
         */
        def apply(qt: QueryTerm[T]) = new ShapeQuery(qt, Nil, Query())
    }

    // TODO: Monadic query? http://github.com/alaz/mongo-scala-driver/issues#issue/13
}

sealed case class QueryTerm[+T](val m: Map[String, Any]) {
    def query = Query() ++ m

    def and[B >: T](q: QueryTerm[B]) = {
        def mergeMaps(ms: Map[String,Any]*)(f: (Any, Any) => Any) =
            (Map[String,Any]() /: ms.flatMap{x => x}) { (m, kv) =>
                m + (if (m contains kv._1) kv._1 -> f(m(kv._1), kv._2)
                     else kv)
            }

        def coincidence(v1: Any, v2: Any): Any = (v1, v2) match {
            case (m1: Map[_,_], m2: Map[_,_]) =>
                mergeMaps(m1.asInstanceOf[Map[String,Any]], m2.asInstanceOf[Map[String,Any]]) {coincidence}
//            case (m1: Map[String,Any], m2: Map[String,Any]) =>
//                mergeMaps(m1, m2) {coincidence}
            case _ => v2
        }

        new QueryTerm[T]( mergeMaps(m, q.m) {coincidence} )
    }
}

object QueryTerm {
    def apply[T]() = new QueryTerm[T](Map.empty[String, Any])
    def apply[T](tuple: (String, Any)) = new QueryTerm[T](Map(tuple))
}
