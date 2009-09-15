package com.osinka.mongodb

import com.mongodb._
import Helper._

trait CollectionElement[+T] {
    def shape: DBObject
}

trait ShapedCollection[+T] extends ReadonlyCollection[T] {
    def shape: DBObject

    override def find(q: Query) = super.find(q ++ shape)

    override def firstOption: Option[T] = tryo(underlying.findOne(Query.Empty, shape)).filter{out.isDefinedAt}.map{out}

    override def sizeEstimate = underlying.getCount(Query.Empty, shape)
}

trait ReadonlyElementCollection[+T] extends ReadonlyCollection[T] with ShapedCollection[T] {
    val element: CollectionElement[T]

    def shape = element.shape
}