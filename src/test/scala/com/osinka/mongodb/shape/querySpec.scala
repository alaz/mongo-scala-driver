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

    "Field" should {
        "have DSL" in {
            import java.util.regex.Pattern
            import Pattern._

            val scalaR = "^test$".r
            val javaR = Pattern.compile("^test$")

            CaseUser.name eq_? Const must be_==( QueryTerm( Map("name" -> Const) ) )
            CaseUser.name < Const must be_==( QueryTerm( Map("name" -> Map("$lt" -> Const))) )
            CaseUser.name <= Const must be_==( QueryTerm( Map("name" -> Map("$lte" -> Const))) )
            CaseUser.name > Const must be_==( QueryTerm( Map("name" -> Map("$gt" -> Const))) )
            CaseUser.name >= Const must be_==( QueryTerm( Map("name" -> Map("$gte" -> Const))) )
            CaseUser.name in List(Const) must be_==( QueryTerm( Map("name" -> Map("$in" -> List(Const)))) )
            CaseUser.name.exists_?  must be_==( QueryTerm( Map("name" -> Map("$exists" -> true))) )
            (CaseUser.name ~ scalaR).m.get("name") must beLike {
                case Some(p: Pattern) => p.pattern == javaR.pattern
            }
        }
        "have obey precedence" in {
            CaseUser.name < Const && CaseUser.name > Const must be_==( QueryTerm( Map("name" -> Map("$gt" -> Const)) ) )
        }
    }
    "Shape" should {
        "have DSL" in {
            val qt = (CaseUser.name is_== Const) && CaseUser.ns.exists_?
            qt must haveSuperClass[QueryTerm[CaseUser]]
            qt must be_==( QueryTerm[CaseUser]( Map("name" -> Const, "_ns" -> Map("$exists" -> true))) )

            val q = CaseUser where {CaseUser.name < Const} drop 10 take 10 sortBy CaseUser.name.ascending
            q must haveSuperClass[ShapeQuery[CaseUser]]
            q.query must be_==( Query(Map("name" -> Map("$lt" -> Const)), Some(10), Some(10), Some(Map("name" -> 1))) )

            (CaseUser sortBy CaseUser.name.descending).query.sorting must beSome[DBObject].which{_.get("name") == -1}
        }
        "produce right DBO for regex query" in {
            import java.util.regex.Pattern
            val qt = CaseUser.name ~ "^User3$".r
            val dboRE = ShapeQuery[CaseUser].where(qt).query.query.get("name")
            dboRE must (notBeNull and beLike {
                case p: Pattern => p.pattern == "^User3$"
            })
        }
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

        "apply ==" in {
            val c = CaseUser where {CaseUser.name eq_? "User3"} take 1 in coll
            c must haveSize(1)
        }
        "apply <" in {
            CaseUser where {CaseUser.name < "User3"} in coll must haveSize(23)
        }
        "apply ~" in {
            import java.util.regex.Pattern
            import Pattern._

            CaseUser.name ~ Pattern.compile("user3$", CASE_INSENSITIVE) in coll must haveSize(1)
            CaseUser.name like "^User3$".r in coll must haveSize(1)
        }
        "sort ascending" in {
            val c = CaseUser sortBy CaseUser.name.ascending take 1 in coll
            c must haveSize(1)
            c.firstOption must beSome[CaseUser].which{_.name == "User0"}
        }
        "sort descending" in {
            val c = CaseUser sortBy CaseUser.name.descending take 1 in coll
            c must haveSize(1)
            c.firstOption must beSome[CaseUser].which{_.name == "User9"}
        }
        "sort by two fields" in {
            skip("not implemented")
        }
    }
    "Embedded query" should {
        val dbColl = mongo.getCollection(CollName)
        val coll = dbColl of ComplexType
        val N = 50

        doFirst {
            dbColl.drop
            mongo.requestStart
            for {val obj <- Array.fromFunction(x => new ComplexType(CaseUser("User"+x)))(N) } coll << obj
        }
        doLast {
            mongo.requestDone
            dbColl.drop
        }

        "apply ==" in {
            val c = ComplexType where {ComplexType.user.name eq_? "User3"} take 1 in coll
            c must haveSize(1)
        }
        "apply <" in {
            ComplexType where {ComplexType.user.name < "User3"} in coll must haveSize(23)
        }
        "apply ~" in {
            import java.util.regex.Pattern
            import Pattern._

            ComplexType.user.name ~ Pattern.compile("user3$", CASE_INSENSITIVE) in coll must haveSize(1)
            ComplexType.user.name like "^User3$".r in coll must haveSize(1)
        }
    }
}