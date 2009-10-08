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

    "Field query" should {
        "DSL" in {
//            skip("not impl")
            CaseUser.name is_== Const must be_==( QueryTerm( Map("name" -> Const) ) )
        }
    }
//    "Shape query" should {
//        "DSL" in {
//            val q = CaseUser where {_.name is_< Const}
//            q must haveSuperClass[ShapeQuery[CaseUser, Shape[CaseUser]]]
//            q.query must be_==( Query(Map("name" -> Map("$lt" -> Const))) )
//            (CaseUser.where{_.name is_== Const}).query must be_==( Query(Map("name" -> Const)) )
//        }
//    }
}