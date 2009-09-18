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

    case class CaseUser(var name: String) extends MongoObject

    object CaseUser extends Shape[CaseUser] {
        override def factory(dbo: DBObject) = CaseUser("")

        object name extends scalar[String]("name") {
            def apply(x: CaseUser) = x.name
            def update(x: CaseUser, v: String) { x.name = v }
        }

        override val * = name :: super.*
    }

    class OrdUser extends MongoObject {
        var name: String = _
    }

    object OrdUser extends Shape[OrdUser] {
        object name extends scalar[String]("name") {
            def apply(x: OrdUser): String = x.name
            def update(x: OrdUser, v: String): Unit = x.name = v
        }

        override val * = name :: super.*
    }

    "Shape serializer" should {
        "translate object to DBObject / case" in {
            val dbo = BasicDBObjectBuilder.start.get
            CaseUser(dbo) = CaseUser("John Doe")
            dbo.get("name") must be_==("John Doe")
        }
        "translate object to DBObject / ord" in {
            val dbo = BasicDBObjectBuilder.start.get
            val u = new OrdUser
            u.name = "John Doe"
            OrdUser(dbo) = u
            dbo.get("name") must be_==("John Doe")
        }
        "translate DBObject to object / case" in {
            skip("not implemented")
        }
        "translate DBObject to object / ord" in {
            skip("not implemented")
        }
        "skip readonly fields on write" in {
            skip("not implemented")
        }
        "skip writeonly fields on read" in {
            skip("not implemented")
        }
    }

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