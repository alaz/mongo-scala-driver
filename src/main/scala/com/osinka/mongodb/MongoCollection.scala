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

import com.mongodb._
import wrapper._
import Preamble._

trait MongoCollection[T] extends PartialFunction[ObjectId, T] with Collection[T] with DBCollectionWrapper {
    def serializer: Serializer[T]

    protected def cursor(q: Query) = {
        val cursor = find(q.query)
        for {val n <- q.skip } cursor.skip(n)
        for {val n <- q.limit} cursor.limit(n)
        for {val sort <- q.sorting} cursor.sort(sort)
        // TODO: snapshot mode
        cursor
    }

    protected def find(q: Query): Iterator[T] =
        new DBObjectIterator(cursor(q)).flatMap{serializer.out(_).toList.elements}

    protected def findOne(q: Query): Option[T] =
        if (q.slice_?) find(q take 1).collect.firstOption
        else tryo(findOne(q.query)).flatMap{serializer.out}

    protected def getCount(q: Query): Long = {
        def lim(n: Int) = q.limit map{_ min n} getOrElse n
        def skp(n: Int) = q.skip map{x => (n - x) max 0} getOrElse n

        if (q.slice_?) lim(skp(cursor(q).count))
        else getCount(q.query)
    }

    protected def update(q: DBObject, op: DBObject, multi: Boolean): Boolean = {
        underlying.update(q, op, false, multi)
        underlying.getDB.getLastError get "updatedExisting" match {
            case null => false
            case b: java.lang.Boolean => b.booleanValue
        }
    }

    def find: Iterator[T] = find(Query.empty)

    // Rough size estimates the collection size: it does not take object shape into account
    def sizeEstimate = getCount(Query.empty)

    def <<(x: T) {
        val dbo = serializer.in(x)
        underlying insert dbo
        serializer.mirror(x)(dbo)
    }

    def <<?(x: T): Option[T] = {
        val dbo = serializer.in(x)
        underlying insert dbo
        underlying.getDB.getLastError get "err" match {
            case null => Some( serializer.mirror(x)(dbo) )
            case msg: String => None
        }
    }

    def +=(x: T) {
        val dbo = serializer.in(x)
        underlying save dbo
        serializer.mirror(x)(dbo)
    }

    def -=(x: T) { underlying remove serializer.in(x) }

    // TODO: update -> foreach?..
    def update(q: Query, op: Map[String,Any], multi: Boolean): Boolean = update(q.query, op, multi)

    // -- PartialFunction[ObjectId, T]
    override def isDefinedAt(oid: ObjectId) = getCount(Query byId oid) > 0

    override def apply(oid: ObjectId) = find(Query byId oid).next

    // -- Collection[T]
    override def elements: Iterator[T] = find

    def firstOption = headOption

    def headOption: Option[T] = findOne(Query.empty)

    /**
     * Size of the collection
     *
     * NOTE: Original MongoDB cursor reports collection's size regardless
     * of skip and limit modificators. This implementation takes these into
     * account: you may expect to get accurate collection length when
     * you called drop and take on Query object.
     *
     * Beware: narrowing as Long value of getCount is cast to Int
     */
    override def size: Int = length
    def length: Int = sizeEstimate.toInt

    override def stringPrefix: String = "MongoCollection"
    override def toString = stringPrefix+"("+getName+"):"+size
}