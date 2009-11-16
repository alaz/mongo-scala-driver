package com.osinka.mongodb.benchmark

import com.mongodb._
import scala.testing._
import Config._
import com.osinka.mongodb._
import Preamble._

/**
 * Serialization overhead benchmark
 *
 * The idea is to insert N objects into a collection and retrieve it using
 * pure Java API and using shapes. No sorting, no indexes -- I'd like to
 * measure de-serialization overhead only
 */
object SerializationOverhead extends BenchmarkSuite("Serialization Overhead") { suite =>
    override val benchmarks = List(JavaRead, DBORead, ShapeRead)

    val constraint = Map("a" -> Map("$exists" -> true))

    var collectionSize: Int = _

    val mongo = new Mongo(Host, Port).getDB(Database)
    def collection = mongo.getCollection("deserialize")

    override def setUp(collSize: Int) {
        suite.collectionSize = collSize

        val coll = collection.asScala
        coll.drop
        for {val i <- 0 until collSize} coll += Map("a" -> i)
    }
    
    override def tearDown {
        collection.drop
    }

    object JavaRead extends Benchmark with SUnit.Assert {
        override val prefix = "reading using Java"
        def run {
            val cursor = collection.find(constraint)
            var i = 0
            while (cursor.hasNext) {
                val dbo = cursor.next
                assertEquals("Object field", i, dbo.get("a"))
                i += 1
            }
            assertEquals("complete walk through the collection", collectionSize, i)
        }
    }

    object DBORead extends Benchmark with SUnit.Assert {
        override val prefix = "reading from Scala collection"
        def run {
            var i = 0
            for {val dbo <- Query(constraint) in collection.asScala} {
                assertEquals("Object field", i, dbo.get("a"))
                i += 1
            }
            assertEquals("complete walk through the collection", collectionSize, i)
        }
    }

    object ShapeRead extends Benchmark with SUnit.Assert {
        override val prefix = "reading Shapes"
        def run {
            var i = 0
            for {val t <- collection of T1} {
                assertEquals("Object field", i, t.a)
                i += 1
            }
            assertEquals("complete walk through the collection", collectionSize, i)
        }
    }
}