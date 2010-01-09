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

package com.osinka.mongodb

import com.mongodb.{DBObject, BasicDBObject, ObjectId}
import wrapper._
import Preamble._

case class Query(final val query: DBObject,
                 val skip: Option[Int],
                 val limit: Option[Int],
                 val sorting: Option[DBObject]) {

    def slice_? = skip.isDefined || limit.isDefined || sorting.isDefined

    def drop(n: Option[Int]) = Query(query, n, limit, sorting)

    def take(n: Option[Int]) = Query(query, skip, n, sorting)

    def drop(n: Int): Query = drop(Some(n))

    def take(n: Int): Query = take(Some(n))

    def sort(s: Option[DBObject]): Query = Query(query, skip, limit, s)

    def sort(s: DBObject): Query = sort(Some(s))

    def *(q: Query): Query = ++(q.query) drop q.skip take q.limit sort q.sorting

    def ++(q: DBObject): Query = Query(DBO.merge(query, q), skip, limit, sorting)
}

object Query {
    final val empty = Query(DBO.empty, None, None, None)

    def apply(): Query = empty
    def apply(q: DBObject) = new Query(q, None, None, None)

    def byId(oid: ObjectId) = apply(DBO.fromMap(Map("_id" -> oid)))
}

trait QueriedCollection[T, Self <: QueriedCollection[T, Self]] extends MongoCollection[T] {
    def query: Query

    def applied(q: Query): Self

    // -- MongoCollection[T]
    override def find = find(query)
    override def headOption = findOne(query)
    override def sizeEstimate = getCount(query)
}