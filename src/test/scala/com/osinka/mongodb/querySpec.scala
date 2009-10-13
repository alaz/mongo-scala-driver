package com.osinka.mongodb

import org.specs._
import org.specs.runner._
import com.mongodb._

import Preamble._
import Config._

class queryTest extends JUnit4(querySpec) with Console
object queryTestRunner extends ConsoleRunner(querySpec)

object querySpec extends Specification {
    val mongo = new Mongo(Host, Port).getDB(Database)

    doAfter { mongo.dropDatabase }

    "Query" should {
        "be empty initially" in {
            val q = Query()
            q.query must be_==(new BasicDBObject)
            q.skip must beNone
            q.limit must beNone
        }
        "store DBObject query" in {
            val q = Query(Map("a" -> 1), Some(1), Some(1))
            q.query.get("a") must (notBeNull and be_==(1))
            q.skip must beSome[Int].which{_ == 1}
            q.limit must beSome[Int].which{_ == 1}
        }
        "skip" in {
            val q = Query()
            (q drop None).skip must beNone
            (q drop 10 drop 2).skip must beSome[Int].which{_ == 2}
            (q drop 1 drop None).skip must beNone
        }
        "limit" in {
            val q = Query()
            (q take None).limit must beNone
            (q take 10 take 2).limit must beSome[Int].which{_ == 2}
            (q take 1 take None).limit must beNone
        }
        "() ++" in {
            val q = Query() ++ Map("a" -> 1)
            q.query.get("a") must (notBeNull and be_==(1))
            q.skip must beNone
            q.limit must beNone

        }
        "(a,s,l) ++" in {
            val q = Query(Map("a" -> 1), Some(1), Some(1)) ++ Map("b" -> 1)
            q.query.get("a") must (notBeNull and be_==(1))
            q.query.get("b") must (notBeNull and be_==(1))
            q.skip must beSome[Int].which{_ == 1}
            q.limit must beSome[Int].which{_ == 1}
        }
        "++ with same key" in {
            val q = Query(Map("a" -> 1), Some(1), Some(1)) ++ Map("a" -> 10)
            q.query.get("a") must (notBeNull and be_==(10))
            q.skip must beSome[Int].which{_ == 1}
            q.limit must beSome[Int].which{_ == 1}
        }
        "*" in {
            val q = Query(Map("a" -> 1, "b" -> 2), Some(1), Some(2)) * Query(Map("b" -> 10, "c" -> 3), Some(2), Some(5))
            q.query.get("a") must (notBeNull and be_==(1))
            q.query.get("b") must (notBeNull and be_==(10))
            q.query.get("c") must (notBeNull and be_==(3))
            q.skip must beSome[Int].which{_ == 2}
            q.limit must beSome[Int].which{_ == 5}
        }
    }

    "Query(coll)" should {
        val coll = mongo.getCollection("test").asScala

        doFirst {
            mongo.requestStart
            coll.drop
            for (val o <- Array.fromFunction(i => Map("a" -> i))(5)) coll save o
        }
        doLast  {
            mongo.requestDone
            coll.drop
        }

        "support DSL" in {
            val q = Query() drop 1 take 2
            q must be_==( Query(Helper.emptyDBO, Some(1), Some(2)) )
            (q in coll).query must be_==(q)
        }
        "apply to DBObjectCollection" in {
            val c = Query() in coll
            c must haveSuperClass[DBObjectCollection]
            (Query() in c) must haveSuperClass[DBObjectCollection]
            c must haveSize(5)
            c.elements.collect must haveSize(5)
        }
        "support skip" in {
            (Query() drop 1 in coll).elements.collect must haveSize(4)
            Query() drop 1 in coll must haveSize(4)
            (Query() drop 1 in coll).elements.collect must haveSize(4)
            Query() drop 1 drop 1 in coll must haveSize(4)
            Query() drop 5 in coll must beEmpty
            Query() drop 6 in coll must haveSize(0)
            (Query() drop 6 in coll).elements.collect must beEmpty
        }
        "support limit" in {
            Query() take 1 in coll must haveSize(1)
            Query() drop 1 take 2 in coll must haveSize(2)
            Query() take 2 drop 1 in coll must haveSize(2)
            (Query() take 2 drop 1 in coll).elements.collect must haveSize(2)
        }
    }
}