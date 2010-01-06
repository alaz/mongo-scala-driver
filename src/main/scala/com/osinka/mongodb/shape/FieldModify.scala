package com.osinka.mongodb.shape

import com.osinka.mongodb._
import Preamble._
import wrapper.MongoOp

trait FieldModifyOperations[T, QueryType] { shape: ShapeFields[T, QueryType] =>

    trait FieldModifyOp[A] { self: FieldContent[A] with FieldInHierarchy =>
        protected def mkOp(f: (String,Any) => (String,Any), x: Option[Any]) =
            x map {v => ModifyOp[QueryType](f(longFieldName, v)) } getOrElse ModifyOp[QueryType]()

        def set(x: A): ModifyOp[QueryType] = mkOp(MongoOp.set, serialize(x))
        def unset: ModifyOp[QueryType] = mkOp(MongoOp.unset, Some(1))
    }

    trait ScalarFieldModifyOp[A] extends FieldModifyOp[A] { self: MongoScalar[A] with ScalarContent[A] =>
        def inc(x: A): ModifyOp[QueryType] = mkOp(MongoOp.inc, serialize(x))
    }

    trait ArrayFieldModifyOp[A] extends FieldModifyOp[A] { self: MongoArray[A] with FieldContent[A] =>
        def push(x: A): ModifyOp[QueryType] = mkOp(MongoOp.push, serialize(x))
        def pushAll(x: Iterable[A]): ModifyOp[QueryType] = mkOp(MongoOp.pushAll, Some(x flatMap { serialize }) )
        def popFirst: ModifyOp[QueryType] = mkOp(MongoOp.pop, Some(-1))
        def popLast: ModifyOp[QueryType] = mkOp(MongoOp.pop, Some(1) )
        def pull(x: A): ModifyOp[QueryType] = mkOp(MongoOp.pull, serialize(x))
        def pullAll(x: Iterable[A]): ModifyOp[QueryType] = mkOp(MongoOp.pullAll, Some(x flatMap { serialize }) )
    }
}

// TODO: Monadic query? http://github.com/alaz/mongo-scala-driver/issues#issue/13
// TODO: unified ModifyOp with QueryTerm??
sealed case class ModifyOp[+T](val m: Map[String, Any]) {
    def query = Query() ++ m
    def and[B >: T](q: ModifyOp[B]) = new ModifyOp[T](m ++ q.m)
}

object ModifyOp {
    def apply[T]() = new ModifyOp[T](Map.empty[String, Any])
    def apply[T](tuple: (String, Any)) = new ModifyOp[T](Map(tuple))
}

sealed class UpdateOp[T](filters: QueryTerm[T])
case class one[T](filters: QueryTerm[T]) extends UpdateOp(filters)
case class all[T](filters: QueryTerm[T]) extends UpdateOp(filters)