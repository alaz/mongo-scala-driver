package com.osinka.mongodb.shape

import org.specs._
import com.mongodb._

import Preamble._
import Config._

object collectionSpec extends Specification("Shape collection") {
    val CollName = "test"
    val Const = "John Doe"

    val mongo = new Mongo(Host, Port).getDB(Database)
    val dbColl = mongo.getCollection(CollName)

    doAfter { mongo.dropDatabase }

    "Collection of class" should {
        doBefore { dbColl.drop; mongo.requestStart }
        doAfter  { mongo.requestDone; dbColl.drop }

        "retrieve" in {
            dbColl save Map("name" -> Const)
            val coll = dbColl.of(OrdUser)
            coll must haveSuperClass[ShapedCollection[OrdUser]]
            coll.headOption must beSome[OrdUser].which{x => x.name == Const && x.mongoOID != None && x.mongoNS == Some(CollName)}
        }
        "store" in {
            val coll = dbColl.of(OrdUser)
            val u = new OrdUser
            u.name = Const

            val r = coll += u
            r must haveClass[OrdUser]
            r.name must be_==(u.name)
            r.mongoOID must beSome[ObjectId]

            coll.headOption must beSome[OrdUser].which{x =>
                x.name == Const &&
                x.mongoOID != None &&
                x.mongoOID == r.mongoOID &&
                x.mongoNS == Some(CollName)
            }
        }
    }
    "Collection of case class" should {
        doBefore { dbColl.drop; mongo.requestStart }
        doAfter  { mongo.requestDone; dbColl.drop }

        "retrieve" in {
            dbColl save Map("name" -> Const)
            val coll = dbColl.of(CaseUser)
            coll must haveSuperClass[ShapedCollection[CaseUser]]
            coll.headOption must beSome[CaseUser].which{x => x.name == Const && x.mongoOID != None && x.mongoNS == Some(CollName)}
        }
        "store" in {
            val coll = dbColl.of(CaseUser)
            coll += CaseUser(Const)
            coll.headOption must beSome[CaseUser].which{x => x.name == Const && x.mongoOID != None && x.mongoNS == Some(CollName)}
        }
    }
    "Collection of complex" should {
        doBefore { dbColl.drop; mongo.requestStart }
        doAfter  { mongo.requestDone; dbColl.drop }

        "store/retrieve" in {
            val coll = dbColl.of(ComplexType)
            val c = new ComplexType(CaseUser(Const), 1)

            val r = coll += c
            r must haveClass[ComplexType]
            r.user must be_==(c.user)
            r.mongoOID must beSome[ObjectId]

            coll.headOption must beSome[ComplexType].which{x =>
                x.user == CaseUser(Const) &&
                x.messageCount == 1 &&
                x.mongoOID == r.mongoOID
            }
        }
    }
    "Collection of Optional" should {
        val N = 10

        doBefore { dbColl.drop; mongo.requestStart }
        doAfter  { mongo.requestDone; dbColl.drop }

        val coll = dbColl of OptModel
        
        "store" in {
            Helper.fillWith(coll, N) {i =>
                val c = new OptModel(i, if (i % 3 == 0) Some("d"+i) else None)
                if (i % 4 == 0) c.comment = Some("comment"+i)
                c
            }
            coll must haveSize(N)
            coll.headOption must beSome[OptModel]
        }
    }
    "Collection of ref" should {
        object RefModel extends RefModelShape(mongo, "users")

        val users = mongo.getCollection("users") of CaseUser
        val posts = mongo.getCollection("posts") of RefModel

        var user: CaseUser = CaseUser(Const)
        doBefore {
            users.drop; posts.drop; mongo.requestStart
            users << user
            posts += new RefModel("text", user)
        }
        doAfter  { mongo.requestDone; users.drop; posts.drop }

        "user has oid" in {
            user.mongoOID must beSome[ObjectId]
        }
        "save post with user ref" in {
            val dbo = mongo.getCollection("posts").asScala.headOption
            dbo must beSome[DBObject]
            dbo.get.get("user") must (notBeNull and haveSuperClass[DBObject])

            val userDbo = dbo.get.get("user").asInstanceOf[DBObject]
            tryo(userDbo.get("_ref")) must be_==(Some("users"))
            tryo(userDbo.get("_id")) must be_==(user.mongoOID)
        }
        "retrieve user from ref" in {
            posts.headOption must beSome[RefModel].which{_.user == user}
        }
    }
}