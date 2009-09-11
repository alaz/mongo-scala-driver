package com.osinka.mongodb

import org.specs._
import org.specs.runner._
import com.mongodb._

import Preamble._

import com.osinka.mongodb.Config._

class collectionTest extends JUnit4(collectionSpec) with Console
object collectionTestRunner extends ConsoleRunner(collectionSpec)

object collectionSpec extends Specification("Scala way Mongo collections") {
    val mongo: Mongo = new Mongo(DbAddress)

    doAfter { mongo.dropDatabase }

    "Empty DBOCollection" should {
        val coll = wrap(mongo.getCollection("test1"))

        "have proper inheritance" in {
            coll must haveSuperClass[Iterable[DBObject]]
            coll must haveSuperClass[Collection[DBObject]]
        }
        "support Iterable methods" in {
            coll.isEmpty must beTrue
        }
        "support Collection methods" in {
            coll.size must be_==(0)
            coll.firstOption must beNone
        }
    }
    "DBOCollection" should {
        val dbColl = mongo.getCollection("test")
        val coll = wrap(dbColl)

        doBefore { mongo.requestStart }
        doAfter  { mongo.requestDone; coll.drop }

        "degrade to DBCollection" in {
            coll.getCount must be_==(0)
        }
        "be equal only when DBCollection equals" in {
            wrap(dbColl) must be_==(coll)
            wrap(mongo.getCollection("test1")) must be_!=(coll)
        }
        "support insert" in {
            coll insert BasicDBObjectBuilder.start("key", 10).get
        }
    }
}
