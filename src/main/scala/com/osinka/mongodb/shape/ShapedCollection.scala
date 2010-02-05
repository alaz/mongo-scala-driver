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

import com.mongodb.{DBCollection, DBObject}
import com.osinka.mongodb._
import wrapper._

/**
 * Collecton of type T elements, where serializer is an ObjectShape
 */
class ShapedCollection[T](override val underlying: DBCollection, val shape: ObjectShape[T])
        extends MongoCollection[T]
        with QueriedCollection[T, ShapedCollection[T]] {

    private lazy val shapeConstraints = DBO.fromMap(shape.constraints.m)
    private def embedShapeConstraints(q: DBObject) = DBO.merge(shapeConstraints, q)

    /**
     * Update elements
     * @param multi should update all elements
     */
    def update(filters: QueryTerm[T], op: ModifyOp[T], multi: Boolean): Boolean = update(filters.query, op.m, multi)

    /**
     * Update only one element
     */
    def updateOne(filters: QueryTerm[T], op: ModifyOp[T]): Boolean = update(filters, op, false)

    /**
     * Update all matching elements
     */
    def update(filters: QueryTerm[T], op: ModifyOp[T]): Boolean = update(filters, op, true)

    // -- MongoCollection[T]
    override val serializer: Serializer[T] = shape

    // -- QueriedCollection[T]
    override val query: Query = Query.empty
    override def applied(q: Query): ShapedCollection[T] = new ShapedCollection[T](underlying, shape) {
        override val query = q
    }

    // -- MongoCollection
    override def find(q: DBObject) = underlying.find(embedShapeConstraints(q))
    override def findOne(q: DBObject) = underlying.findOne(embedShapeConstraints(q))
    override def getCount(q: DBObject) = find(q).count
    override def update(q: DBObject, op: DBObject, multi: Boolean) = super.update(embedShapeConstraints(q), op, multi)

    override def stringPrefix: String = "ShapedCollection["+shape.getClass.getName+"]("+getName+")"
}
