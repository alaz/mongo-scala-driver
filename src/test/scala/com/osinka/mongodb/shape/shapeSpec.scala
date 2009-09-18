package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb._

import Preamble._
import Config._

class shapeTest extends JUnit4(shapeSpec) with Console
object shapeTestRunner extends ConsoleRunner(shapeSpec)

object shapeSpec extends Specification("Scala way Mongo shapes") {
    val mongo: Mongo = new Mongo(DbAddress)

    doAfter { mongo.dropDatabase }

    "Shaped collection" should {
        val dbColl = mongo.getCollection("test")

        doBefore {
            dbColl.drop; mongo.requestStart
            dbColl save Map("name" -> "John Doe")
        }
        doAfter  { mongo.requestDone; dbColl.drop }

        "retrieve ordinary class objects" in {
            val coll = dbColl.of(OrdUser)
            coll must haveSuperClass[ShapedCollection[OrdUser]]
        }
        "retrieve case class objects" in {
            val coll = dbColl.of(CaseUser)
            coll must haveSuperClass[ShapedCollection[CaseUser]]
        }
    }
}