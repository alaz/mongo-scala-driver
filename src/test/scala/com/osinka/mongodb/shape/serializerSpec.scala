package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb.{DBObject, BasicDBObjectBuilder}

import Preamble._

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    val Const = "John Doe"

    object CaseUserSerializer extends ShapedSerializer[CaseUser] {
        val element = CaseUser
    }

    "Field shape" should {
        "serialize AnyVals" in {
            skip("not implemented")
        }
        "serialize Strings" in {
            skip("not implemented")
        }
        "serialize Dates" in {
            skip("not implemented")
        }
        "serialize Maps" in {
            skip("not implemented")
        }
        "serialize Arrays" in {
            skip("not implemented")
        }
    }

    "Object Shape" should {
         val jd = createDBObject( Map("name" -> Const) )

        "serialize object to DBObject / case" in {
            val dbo = BasicDBObjectBuilder.start.get
            CaseUser(dbo) = CaseUser(Const)
            dbo.get("name") must be_==(Const)
        }
        "serialize object to DBObject / ord" in {
            val dbo = BasicDBObjectBuilder.start.get
            val u = new OrdUser
            u.name = Const
            OrdUser(dbo) = u
            dbo.get("name") must be_==(Const)
        }
        "serialize DBObject to object / case" in {
            val u = CaseUser(Const)
            CaseUser(jd) must be_==(u)
        }
        "serialize DBObject to object / ord" in {
            val u = CaseUser(jd)
            u.name must be_==(Const)
        }
        "serialize complex object to DBObject" in {
            val dbo = BasicDBObjectBuilder.start.get
            val c = new ComplexType
            c.user = CaseUser(Const)
            ComplexType(dbo) = c
            dbo.get("user") must haveSuperClass[DBObject]
            dbo.get("user").asInstanceOf[DBObject].get("name") must be_==(Const)
        }
        "v DBObject to complex object" in {
            val dbo = createDBObject( Map("user" -> jd) )
            val c = ComplexType(dbo)
            c.user must notBeNull
            c.user.name must be_==(Const)
        }
        "not include _id and _ns into DBO" in {
            val shape = CaseUser.shape
            shape.get("name") must be_==(1)
            shape.get("_id") must beNull
            shape.get("_ns") must beNull
        }
        "mirror mongo fields back to object" in {
            import com.mongodb.ObjectId

            val dbo = BasicDBObjectBuilder.start.get
            dbo.putAll(jd)

            val u = CaseUser(dbo)
            u.name must be_==(Const)
            u.mongoOID must beNull

            dbo.put("_id", ObjectId.get)
            CaseUserSerializer.mirror(u)(dbo)
            u.mongoOID must be_==(dbo.get("_id"))
        }
        "skip readonly fields on write" in {
            skip("not implemented")
        }
        "skip writeonly fields on read" in {
            skip("not implemented")
        }
    }
}