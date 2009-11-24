package com.osinka.mongodb.shape

import org.specs._
import java.util.Date
import com.mongodb.{DBObject, BasicDBObjectBuilder}

import com.osinka.mongodb._
import Preamble._
import wrapper.DBO

object serializerSpec extends Specification {
    val Const = "John Doe"

    object IntS extends TSerializer[Int]( () => Holder[Int](99) )
    object StringS extends TSerializer[String]( () => Holder[String]("init") )
    object DateS extends TSerializer[Date]( () => Holder[Date](new Date))

    "Field shape" should {
        "serialize AnyVals" in {
            IntS.i.valueOf(Holder[Int](1)) must be_==(1)

            val h = Holder[Int](10)
            IntS.i(h) = 1
            h.value must be_==(1)
        }
    }
    "DBObject Shape" should {
        "serialize Ints" in {
            BasicDBObjectBuilder.start("i", 1).get must beLike {
                case IntS(o) => o.value == 1
            }

            val dbo2 = IntS( Holder[Int](1) )
            dbo2.get("i") must (notBeNull and be_==(1))
        }
        "serialize Strings" in {
            BasicDBObjectBuilder.start("i", "test").get must beLike {
                case StringS(o) => o.value == "test"
            }

            val dbo2 = StringS( Holder[String]("test") )
            dbo2.get("i") must (notBeNull and be_==("test"))
        }
        "serialize Dates" in {
            BasicDBObjectBuilder.start("i", new Date(1)).get must beLike {
                case DateS(o) => o.value == new Date(1)
            }

            val dbo2 = DateS( Holder[Date](new Date(1)) )
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
         val jd = DBO.fromMap( Map("name" -> Const) )

        "serialize object to DBObject / case" in {
            val dbo = CaseUser pack new CaseUser(Const)
            dbo.get("name") must be_==(Const)
        }
        "serialize object to DBObject / ord" in {
            val u = new OrdUser
            u.name = Const
            val dbo = OrdUser pack u
            dbo.get("name") must be_==(Const)
        }
        "serialize DBObject to object / case" in {
            CaseUser.extract(jd) must beSome[CaseUser].which{_.name == Const}
        }
        "serialize DBObject to object / ord" in {
            OrdUser.extract(jd) must beSome[OrdUser].which{_.name == Const}
        }
        "serialize complex object to DBObject" in {
            val dbo = ComplexType pack new ComplexType(CaseUser(Const))
            dbo.get("user") must haveSuperClass[DBObject]
            dbo.get("user").asInstanceOf[DBObject].get("name") must be_==(Const)
        }
        "DBObject to complex object" in {
            val dbo = DBO.fromMap( Map("user" -> jd) )
            val c = ComplexType extract dbo
            c must beSome[ComplexType]
            c.get.user must notBeNull
            c.get.user.name must be_==(Const)
        }
        "not include _id and _ns into DBO" in {
            val shape = CaseUser.constraints
            shape must haveSuperClass[Map[String, Map[String,Boolean]]]
            shape.get("name") must beSome[Map[String,Boolean]].which{_.get("$exists") == Some(true)}
            shape.get("_id") must beNone
            shape.get("_ns") must beNone
        }
        "mirror mongo fields back to object" in {
            import com.mongodb.ObjectId

            val dbo = DBO.empty
            dbo.putAll(jd)

            val u = CaseUser extract dbo
            u must beSome[CaseUser]
            u.get.name must be_==(Const)
            u.get.mongoOID must beNull

            dbo.put("_id", ObjectId.get)
            CaseUser.mirror(u.get)(dbo)
            u.get.mongoOID must be_==(dbo.get("_id"))
        }
        "skip readonly fields on write" in {
            skip("not implemented")
        }
        "skip writeonly fields on read" in {
            skip("not implemented")
        }
    }
}