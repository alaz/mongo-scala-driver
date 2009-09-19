package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb._

import Preamble._
import Config._

class shapeTest extends JUnit4(shapeSpec) with Console
object shapeTestRunner extends ConsoleRunner(shapeSpec)

object shapeSpec extends Specification("Scala way Mongo shapes") {
    val CollName = "test"
    val Const = "John Doe"

    val mongo: Mongo = new Mongo(DbAddress)

    doAfter { mongo.dropDatabase }

    "Shaped collection" should {
        val dbColl = mongo.getCollection(CollName)

        doBefore { dbColl.drop; mongo.requestStart }
        doAfter  { mongo.requestDone; dbColl.drop }

        "retrieve objects / ord" in {
            dbColl save Map("name" -> Const)
            val coll = dbColl.of(OrdUser)
            coll must haveSuperClass[ShapedCollection[OrdUser]]
            coll.firstOption must beSome[OrdUser].which{x => x.name == Const && x.mongoOID != null && x.mongoNS == CollName}
        }
        "retrieve objects / case" in {
            dbColl save Map("name" -> Const)
            val coll = dbColl.of(CaseUser)
            coll must haveSuperClass[ShapedCollection[CaseUser]]
            coll.firstOption must beSome[CaseUser].which{x => x.name == Const && x.mongoOID != null && x.mongoNS == CollName}
        }
        "store objects / case" in {
            val coll = dbColl.of(CaseUser)
            coll save CaseUser(Const)
            coll.firstOption must beSome[CaseUser].which{x => x.name == Const && x.mongoOID != null && x.mongoNS == CollName}
        }
        "store objects / ord" in {
            val coll = dbColl.of(OrdUser)
            val u = new OrdUser
            u.name = Const
            coll save u
            coll.firstOption must beSome[OrdUser].which{x => x.name == Const && x.mongoOID != null && x.mongoNS == CollName}
        }
    }
}