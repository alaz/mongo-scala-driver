package com.osinka.mongodb.orig

import org.specs._
import org.specs.runner._
import com.mongodb._

import com.osinka.mongodb.Config._

class plainTest extends JUnit4(plainSpec) with Console
object plainTestRunner extends ConsoleRunner(plainSpec)

object plainSpec extends Specification {
    val mongo = new Mongo(DbAddress)

    doAfter { mongo.dropDatabase }

    "Plain collection" should {
        val coll = mongo.getCollection("test")

        doBefore { mongo.requestStart }
        doAfter  { mongo.requestDone; coll.drop }

        "have zero size by default" in {
            coll.getCount must be_==(0)
        }
        "accept DBObjects" in {
            coll.getCount must be_==(0)
            coll insert BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)
        }
        "return same DBObject" in {
            val inserted = coll save BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)

            val o = coll.findOne
            o must be_==(inserted)
            o.get("key") must be_==(100)
        }
        "remove DBObjects by object" in {
            val o = coll save BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)

            coll.remove(o)
        }
        "'save' should replace object" in {
            coll.getCount must be_==(0)

            val o = coll save BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)

            o.put("key", 200)
            coll save o
            coll.getCount must be_==(1)

            val r = coll.findOne
            r must notBeNull
            r.get("key") must be_==(200)
        }
        "not insert duplicate id" in {
            coll.getCount must be_==(0)

            val o = coll save BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)

            o.put("key", 200)
            coll insert o
            mongo.getLastError aka "error on duplicate key insert" must notBeNull
            
            coll.getCount must be_==(1)
            val r = coll.findOne
            r must notBeNull
            r.get("key") must be_==(100)
        }
        "group" in {
            skip("TODO: group spec")
        }
    }
    "Index" should {
        skip("TODO: indexing spec")
    }
    "Query" should {
        val coll = mongo.getCollection("test")

        doBefore { mongo.requestStart }
        doAfter  { mongo.requestDone; coll.drop }

        "count by query" in {
            coll save BasicDBObjectBuilder.start("a", "value").get
            coll getCount BasicDBObjectBuilder.start("a", "value").get must be_==(1)

            coll save BasicDBObjectBuilder.start(
                "a", BasicDBObjectBuilder.start("b", "other").get
            ).get
            coll getCount BasicDBObjectBuilder.start("a.b", "other").get must be_==(1)
            coll getCount BasicDBObjectBuilder.start(
                "a", BasicDBObjectBuilder.start("b", "other").get
            ).get must be_==(1)
        }
        "count by query and shape" in {
            coll save BasicDBObjectBuilder.start("a", "value").get
            coll save BasicDBObjectBuilder
                .start("a", "value")
                .append("b", BasicDBObjectBuilder.start("c", "other").get).get
            coll.getCount(BasicDBObjectBuilder.start("a", "value").get,
                          BasicDBObjectBuilder.start("b",
                                BasicDBObjectBuilder.start("c", 1).get
                         ).get) must be_==(1)
        }
        "update by query" in {
            skip("TODO: update")
        }
        "lookup by query" in {
            skip("TODO: lookup")
        }
        "remove DBObjects by query" in {
            val o = coll save BasicDBObjectBuilder.start("key", 100).get
            coll.getCount must be_==(1)

            coll.remove(o)
            coll.getCount must be_==(0)
        }
    }
    "DBObject serialization" should {
        "create DBO from Map" in {
            import scala.collection.jcl._
            val m = Map[String, Any](new java.util.HashMap)
            m += ("a" -> 1, "b" -> 2)

            val dbo = BasicDBObjectBuilder.start(m.underlying).get
            dbo.containsField("a") must beTrue
            dbo.get("a") must be_==(1)
            dbo.containsField("b") must beTrue
            dbo.get("b") must be_==(2)
        }
        "convert Map of Arrays to DBO" in {
            skip("BasicDBObjectBuilder.start(Map) and BasicDBObject.putAll(Map) do not descend, they assume all values to be scalars")

            import scala.collection.jcl._
            val m = Map[String, Any](new java.util.HashMap)
            val a = Array[String]("v1", "v2")
            m += "c" -> a

            val dbo = BasicDBObjectBuilder.start(m.underlying).get
            dbo.containsField("c") must beTrue
            dbo.get("c") must haveSuperClass[DBObject]
            val adbo = dbo.get("c").asInstanceOf[DBObject]
            adbo.get("0") must be_==("v1")
            adbo.get("1") must be_==("v2")
        }
    }
}