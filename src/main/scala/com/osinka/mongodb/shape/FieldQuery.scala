package com.osinka.mongodb.shape

object MongoConditional {
    def op(op: String)(name: String, x: Any) = name -> Map(op -> x)

    def eqTest(name: String, x: Any) = name -> x
    lazy val neTest = op("$ne") _

    lazy val lt = op("$lt") _
    lazy val le = op("$lte") _
    lazy val gt = op("$gt") _
    lazy val ge = op("$gte") _
    lazy val in = op("$in") _
    lazy val nin = op("$nin") _
    lazy val all = op("$all") _
//    def mod
    lazy val size = op("$size") _
    def exists(name: String, b: Boolean) = op("$exists")(name, b)
}

trait FieldCond[-Host, A] { self: Field[Host, A, _] =>
    import MongoConditional._

    def is_<(x: A) = new QueryTerm( lt(name, x) )
    def ?<(x: A) = is_<(x)

    def is_<=(x: A) = new QueryTerm( le(name, x) )
    def ?<=(x: A) = is_<=(x)

    def is_>(x: A) = new QueryTerm( gt(name, x) )
    def ?>(x: A) = is_>(x)

    def is_>=(x: A) = new QueryTerm( ge(name, x) )
    def ?>=(x: A) = is_>=(x)

    def is_==(x: A) = new QueryTerm( eqTest(name, x) )
    def ?==(x: A) = is_==(x)

    def not_==(x: A) = new QueryTerm( neTest(name, x) )
    def ?!=(x: A) = not_==(x)

    def is_in(x: Seq[A]) = new QueryTerm( in(name, x) )
    def in_?(x: Seq[A]) = is_in(x)

    def not_in(x: Seq[A]) = new QueryTerm( nin(name, x) )
    def in_!?(x: Seq[A]) = not_in(x)

    def has_all(x: Seq[A]) = new QueryTerm( all(name, x) )
    def has_?(x: Seq[A]) = has_all(x)

    def has_size(x: Int) = new QueryTerm( size(name, x) )
    def size_?(x: Int) = has_size(x)

    def does_exist = new QueryTerm( exists(name, true) )
    def exists_? = does_exist

    def not_exists = new QueryTerm( exists(name, false) )
    def exists_!? = not_exists
}