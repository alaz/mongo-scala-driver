package com.osinka.mongodb.wrapper

object MongoOp {
    def op[T](op: String)(field: String, x: T) = op -> Map(field -> x)

    lazy val inc = op[Any]("$inc") _
    lazy val set = op[Any]("$set") _
    lazy val unset = op[Any]("$unset") _
    lazy val push = op[Any]("$push") _
    lazy val pushAll = op[Any]("$pushAll") _
    lazy val pop = op[Any]("$pop") _
    lazy val pull = op[Any]("$pull") _
    lazy val pullAll = op[Any]("$pullAll") _
}