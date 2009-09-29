package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import java.util.Date
import com.mongodb.{DBObject, BasicDBObjectBuilder}

import Preamble._

class serializerTest extends JUnit4(serializerSpec) with Console
object serializerTestRunner extends ConsoleRunner(serializerSpec)

object serializerSpec extends Specification {
    val Const = "John Doe"

    object CaseUserSerializer extends ShapedSerializer[CaseUser] {
        val element = CaseUser
    }

    object IntS extends TSerializer[Int]( () => Holder[Int](99) )
    object StringS extends TSerializer[String]( () => Holder[String]("init") )
    object DateS extends TSerializer[Date]( () => Holder[Date](new Date))

    "Field shape" should {
        "serialize AnyVals" in {
            IntS.i(Holder[Int](1)) must be_==(1)

            val h = Holder[Int](10)
            IntS.i(h) = 1
            h.value must be_==(1)
        }
    }
    "DBObject Shape" should {
        "serialize Ints" in {
            val dbo = BasicDBObjectBuilder.start("i", 1).get
            val o = IntS(dbo)
            o.value must be_==(1)

            val dbo2 = BasicDBObjectBuilder.start.get
            IntS(dbo2) = Holder[Int](1)
            dbo2.get("i") must (notBeNull and be_==(1))
        }
        "serialize Strings" in {
            val dbo = BasicDBObjectBuilder.start("i", "test").get
            val o = StringS(dbo)
            o.value must be_==("test")

            val dbo2 = BasicDBObjectBuilder.start.get
            StringS(dbo2) = Holder[String]("test")
            dbo2.get("i") must (notBeNull and be_==("test"))
        }
        "serialize Dates" in {
            val dbo = BasicDBObjectBuilder.start("i", new Date(1)).get
            val o = DateS(dbo)
            o.value must be_==(new Date(1))

            val dbo2 = BasicDBObjectBuilder.start.get
            DateS(dbo2) = Holder[Date](new Date(1))
            dbo2.get("i") must (notBeNull and be_==(new Date(1)))
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