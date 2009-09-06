package com.osinka.mongodb

import org.specs._
import org.specs.runner._
import com.mongodb._

class reflectionTest extends JUnit4(reflectionSpec) with Console
object reflectionTestRunner extends ConsoleRunner(reflectionSpec)

object reflectionSpec extends Specification {
    val TestDB = new DBAddress("127.0.0.1:27017", "test")
    var mongo: Mongo = new Mongo(TestDB)

    doAfter { mongo.dropDatabase }
    "reflection" should {
        val coll = mongo.getCollection("test")

        doBefore { mongo.requestStart }
        doAfter  { mongo.requestDone; coll.drop }

        "zero size" in {
            coll.getCount must be_==(0)
        }
    }
}