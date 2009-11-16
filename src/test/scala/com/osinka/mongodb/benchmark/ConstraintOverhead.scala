package com.osinka.mongodb.benchmark

import com.mongodb._
import scala.testing._
import Config._
import com.osinka.mongodb._
import Preamble._

object ConstraintOverheadNoIndex extends AbstractConstraintOverhead("no indeces") {
    override def ensureIndex(maxArity: Int) {}
}

object ConstraintOverheadWithIndex extends AbstractConstraintOverhead("with indeces") {
    override def ensureIndex(maxArity: Int) {
        // Single index per field
        for {val n <- 0 until maxArity}
            collection ensureIndex Map("f"+n -> 1)
    }
}

/**
 * The idea is to test whether "$exists" constraints of MongoDB lead to
 * the overhead in reading the data or not. We are creating a collection with
 * 10 fields and are requesting the elements using 1-, 5- and 10-fields
 * shapes
 */
abstract class AbstractConstraintOverhead(val extraText: String) extends BenchmarkSuite("Constraints overhead, "+extraText) { suite =>
    override val benchmarks = new FieldRead(1) :: new FieldRead(5) :: new FieldRead(10) :: Nil

    var collectionSize: Int = _

    val mongo = new Mongo(Host, Port).getDB(Database)
    def collection = mongo.getCollection("constraints")

    def ensureIndex(maxArity: Int): Unit

    override def setUp(collSize: Int) {
        suite.collectionSize = collSize

        val coll = collection.asScala
        coll.drop

        val maxArity = benchmarks.map{_.arity}.reduceLeft{_ max _}
        for {val i <- 0 until collSize}
            coll += ( List.range(0,maxArity).map{n => "f"+n -> i*n} foldLeft Map.empty[String,Int] ) {(m,f) => m + f}
        ensureIndex(maxArity)
    }

    override def tearDown {
        collection.drop
    }

    class FieldRead(val arity: Int) extends Benchmark with SUnit.Assert {
        object model extends NFieldsTest(arity)
        import model._

        override val prefix = model.arity+" field(s)"
        def run {
            assertEquals("Model arity", model.T2.`*`.size, model.arity+2)

            var i = 0
            for {val t <- collection of T2} {
                for {val n <- 0 until model.arity}
                    assertEquals("Object field", i*n, t.f(n))
                i += 1
            }
            assertEquals("complete walk through the collection", suite.collectionSize, i)
        }
    }
}