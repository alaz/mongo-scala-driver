package com.osinka.mongodb.orig

import org.specs._
import com.mongodb._

import com.osinka.mongodb._
import Config._

object plainSpec extends Specification {
    val mongo = new Mongo(Host, Port).getDB(Database)

    doAfter  { mongo.dropDatabase }

    "DBObject" should {
        "copy pattern over" in {
            import java.util.regex.Pattern

            val re = Pattern.compile("^re.*")
            val dbo = new BasicDBObject
            dbo putAll BasicDBObjectBuilder.start("a", 3).get
            dbo putAll BasicDBObjectBuilder.start("a", re).get
            dbo.get("a") must (notBeNull and be_==(re))
        }
    }
    "Plain collection" should {
        val coll = mongo.getCollection("test")

        doBefore { coll.drop; mongo.requestStart }
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

        doBefore { coll.drop; mongo.requestStart }
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
            coll save BasicDBObjectBuilder
                .start("a", "value")
                .get
            coll save BasicDBObjectBuilder
                .start("a", "value")
                .push("b")
                    .append("c", "other")
                .get
            coll.find(BasicDBObjectBuilder.start.push("b.c").append("$exists", true).get).count must be_==(1)
//            coll.getCount(BasicDBObjectBuilder.start("a", "value").get,
//                          BasicDBObjectBuilder.start.push("b").append("c", 1).get
//            ) must be_==(1)
        }
        "regexp" in {
            import java.util.regex.Pattern

            val reQuery = BasicDBObjectBuilder.start("a", Pattern.compile(".*es.*")).get
            val shape = BasicDBObjectBuilder.start("a", 1).get

            doBefore { coll save BasicDBObjectBuilder.start("a", "test").get }
            doAfter {}

            "getCount" in {
                coll.getCount(reQuery) must be_==(1)
                coll.getCount(reQuery, shape) must be_==(1)
            }
            "findOne" in {
                coll.findOne(reQuery) must notBeNull
                coll.findOne(reQuery, shape) must notBeNull
            }
            "find" in {
                coll.find(reQuery).count must be_==(1)
                coll.find(reQuery, shape).count must be_==(1)
            }
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
    "DBCursor" should {
        import Preamble._
        val coll = mongo.getCollection("test")

        def collection(f: (DBCursor => DBCursor)) = new wrapper.DBObjectIterator(f(coll.find)).collect
        def count(f: (DBCursor => DBCursor)) = f(coll.find).count

        doFirst {
            coll.drop
            mongo.requestStart
            val gen = Array.fromFunction(i => Map("a" -> ("a"+i) ) ) _
            for (o <- gen(5)) coll save o
        }
        doLast  {
            mongo.requestDone
            coll.drop
        }

        "count" in {
            coll.getCount must be_==(5)
            coll.find.count must be_==(5)

            collection(x => x) must haveSize(5)
            count(x => x) must be_==(5)
        }
        "count regexp" in {
            import java.util.regex.Pattern
            import Pattern._

            val q = Map("a" -> Pattern.compile("a3$", CASE_INSENSITIVE))
            coll.find(q).count must be_==(1)
        }
        "limit" in {
            collection(_ limit -1) mustNot haveSize(5)
            count(_ limit -1) must be_==(5)
            
            collection(_ limit 2) must haveSize(2)
            count(_ limit 2) must be_==(5)
        }
        "skip" in {
            collection(_ skip 0) must haveSize(5)
            count(_ skip 0) must be_==(5)

            collection(_ skip 1) must haveSize(4)
            count(_ skip 1) must be_==(5)

            collection(_ skip 1 limit 2) must haveSize(2)
            count(_ skip 1 limit 2) must be_==(5)
        }
        "sort ascending" in {
            coll.find.count must be_==(5)

            val c = coll.find.sort(Map("a" -> 1)).limit(1)
            c.hasNext must beTrue

            val o = c.next
            o must notBeNull
            o.get("a") must be_==("a0")
        }
        "sort descending" in {
            val c = coll.find.sort(Map("a" -> -1)).limit(1)
            c.hasNext must beTrue

            val o = c.next
            o must notBeNull
            o.get("a") must be_==("a4")
        }
    }
    "DBRef" should {
        import Preamble._
        val coll = mongo.getCollection("test")

        setSequential
        doFirst { coll.drop }
        doLast  { coll.drop }

        "store and fetch" in {
            val subobj = coll save Map("s" -> "other things", "num" -> 100)
            subobj.get("_id") must notBeNull

            val ref = new DBRef(coll.getBase, "test", subobj.get("_id"))

            val obj = coll save Map("object" -> "complex", "sub" -> ref)
            obj.get("_id") must notBeNull

            obj.get("sub") must haveSuperClass[DBRefBase]
            val deref = obj.get("sub").asInstanceOf[DBRefBase]
            val deSubObj = deref.fetch
            deSubObj must notBeNull
            deSubObj.get("_id") must be_==(subobj.get("_id"))
        }
    }
    "DBObject serialization" should {
        "create DBO from Map" in {
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