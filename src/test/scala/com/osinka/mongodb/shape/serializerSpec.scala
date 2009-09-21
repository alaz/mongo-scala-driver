package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb.{DBObject, BasicDBObjectBuilder}

import Preamble._

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    val Const = "John Doe"

    "Shape serializer" should {
         val jd = createDBObject( Map("name" -> Const) )

        "translate object to DBObject / case" in {
            val dbo = BasicDBObjectBuilder.start.get
            CaseUser(dbo) = CaseUser(Const)
            dbo.get("name") must be_==(Const)
        }
        "translate object to DBObject / ord" in {
            val dbo = BasicDBObjectBuilder.start.get
            val u = new OrdUser
            u.name = Const
            OrdUser(dbo) = u
            dbo.get("name") must be_==(Const)
        }
        "translate DBObject to object / case" in {
            val u = CaseUser(Const)
            CaseUser(jd) must be_==(u)
        }
        "translate DBObject to object / ord" in {
            val u = CaseUser(jd)
            u.name must be_==(Const)
        }
        "translate complex object to DBObject" in {
            val dbo = BasicDBObjectBuilder.start.get
            val c = new ComplexType
            c.user = CaseUser(Const)
            ComplexType(dbo) = c
            dbo.get("user") must haveSuperClass[DBObject]
            dbo.get("user").asInstanceOf[DBObject].get("name") must be_==(Const)
        }
        "translate DBObject to complex object" in {
            val dbo = createDBObject( Map("user" -> jd) )
            val c = ComplexType(dbo)
            c.user must notBeNull
            c.user.name must be_==(Const)
        }
        "not include _id and _ns into DBO" in {
            val shape = CaseUser.shape
            shape.get("user") must be_==(1)
            shape.get("_id") must beNull
            shape.get("_ns") must beNull
        }
        "skip readonly fields on write" in {
            skip("not implemented")
        }
        "skip writeonly fields on read" in {
            skip("not implemented")
        }
    }
}