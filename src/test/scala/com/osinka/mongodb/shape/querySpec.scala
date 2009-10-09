package com.osinka.mongodb.shape

import org.specs._
import org.specs.runner._
import com.mongodb._

import Preamble._
import Config._

class queryTest extends JUnit4(querySpec) with Console
object queryTestRunner extends ConsoleRunner(querySpec)

object querySpec extends Specification("Query on Shapes and Fields") {
    val CollName = "test"
    val Const = "John Doe"

    val mongo = new Mongo(Host, Port).getDB(Database)

    doAfter { mongo.dropDatabase }

    "Field DSL" in {
//            skip("not impl")
        CaseUser.name is_== Const must be_==( QueryTerm( Map("name" -> Const) ) )
        CaseUser.name is_< Const must be_==( QueryTerm( Map("name" -> Map("$lt" -> Const))) )
        CaseUser.name is_<= Const must be_==( QueryTerm( Map("name" -> Map("$lte" -> Const))) )
        CaseUser.name is_> Const must be_==( QueryTerm( Map("name" -> Map("$gt" -> Const))) )
        CaseUser.name is_>= Const must be_==( QueryTerm( Map("name" -> Map("$gte" -> Const))) )
        CaseUser.name is_in List(Const) must be_==( QueryTerm( Map("name" -> Map("$in" -> List(Const)))) )
        CaseUser.name.exists_?  must be_==( QueryTerm( Map("name" -> Map("$exists" -> true))) )
    }
    "Shape DSL" in {
        val qt = (CaseUser.name is_== Const) && CaseUser.ns.exists_?
        qt must haveSuperClass[QueryTerm[CaseUser]]
        qt must be_==( QueryTerm[CaseUser]( Map("name" -> Const, "_ns" -> Map("$exists" -> true))) )

        val q = new ShapeQuery(CaseUser) where (CaseUser.name is_< Const)
        q must haveSuperClass[ShapeQuery[CaseUser]]
        q.query must be_==( Query(Map("name" -> Map("$lt" -> Const))) )
//            (CaseUser.where{_.name is_== Const}).query must be_==( Query(Map("name" -> Const)) )
    }
    "Query" should {
        val dbColl = mongo.getCollection(CollName)
        val coll = dbColl of CaseUser
        val N = 50

        doFirst {
            dbColl.drop
            mongo.requestStart
            for {val obj <- Array.fromFunction(x => CaseUser("User"+x))(N) } coll << obj
        }
        doLast {
            mongo.requestDone
            dbColl.drop
        }

        "apply" in {
            val c = new ShapeQuery(CaseUser) where (CaseUser.name eq_? "User3") take 1 in coll
            c must haveSize(1)
        }
    }
}