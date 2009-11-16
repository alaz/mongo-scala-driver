package com.osinka.mongodb.benchmark

import com.mongodb.DBObject
import com.osinka.mongodb.Preamble._
import com.osinka.mongodb.shape._

case class T1(val a: Int) extends MongoObject

object T1 extends AbstractShape[T1] {
    override lazy val * = a :: super.*

    override def factory(dbo: DBObject) = for {val a(x) <- Some(dbo)} yield new T1(x)
    
    object a extends Scalar[Int]("a", _.a) with Functional[Int]
}

class NFieldsTest(val arity: Int) {
    class T2 extends MongoObject {
        val f = new Array[Int](arity)
    }

    object T2 extends Shape[T2] {
        override def factory(dbo: DBObject) = Some(new T2)

        def fieldObj(i: Int) = new Scalar[Int]("f"+i, _.f(i)) with Updatable[Int] {
            def update(o: T2, x: Int) {o.f(i) = x}
        }

        override lazy val * = List.range(0,arity).map(fieldObj) ::: super.*
    }
}