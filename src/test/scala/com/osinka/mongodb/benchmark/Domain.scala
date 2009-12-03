package com.osinka.mongodb.benchmark

import com.mongodb.DBObject
import com.osinka.mongodb.Preamble._
import com.osinka.mongodb.shape._

trait TestObj {
    def a: Int
}

/**
 * T1 is a typical case class with immutable field
 */
case class T1(val a: Int) extends MongoObject with TestObj

object T1 extends MongoObjectShape[T1] {
    override lazy val * = a :: Nil

    override def factory(dbo: DBObject) = for {val a(x) <- Some(dbo)} yield new T1(x)
    
    lazy val a = Scalar("a", _.a)
}

/**
 * T2 does not inherit MongoObject, thus there is no overhead for
 * deserializing MongoDB's OID and NS fields.
 *
 * T2 is completely immutable
 */
class T2(val a: Int) extends TestObj

object T2 extends ObjectShape[T2] {
    override lazy val * = a :: Nil
    override def factory(dbo: DBObject) = for {val a(x) <- Some(dbo)} yield new T2(x)

    lazy val a = Scalar("a", _.a)
}

/**
 * T3 does not inherit MongoObject and does not have functional field, thus
 * uses update to populate the field.
 */
class T3 extends TestObj {
    var a: Int = _
}

object T3 extends ObjectShape[T3] {
    override lazy val * = a :: Nil
    override def factory(dbo: DBObject) = Some(new T3)

    object a extends Scalar[Int]("a", _.a) with Updatable[Int] {
        def update(x: T3, a: Int) { x.a = a }
    }
}

/**
 * Model for arity tests.
 *
 * the model object hosts an array for fields and its companion object
 * is able to update this array
 */
class NFieldsTest(val arity: Int) {
    class Ta extends MongoObject {
        val f = new Array[Int](arity)
    }

    object Ta extends MongoObjectShape[Ta] {
        override lazy val * = List.range(0,arity).map(fieldObj)
        override def factory(dbo: DBObject) = Some(new Ta)
        
        def fieldObj(i: Int) = new Scalar[Int]("f"+i, _.f(i)) with Updatable[Int] {
            def update(o: Ta, x: Int) {o.f(i) = x}
        }

    }
}