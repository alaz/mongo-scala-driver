package com.osinka.mongodb.shape

//case class CaseUserExtended(override val name: String, val age: Int) extends CaseUser(name)
//
//object CaseUserExtended extends CaseUserShape with Shape[CaseUserExtended] {
//    object age extends scalar[Int]("age", _.age) with Functional[Int]
//
//    override val * = age :: super.*
//    override def factory(dbo: DBObject): Option[CaseUserExtended] =
//        for {val name(n) <- Some(dbo)
//             val age(a) <- Some(dbo)} yield CaseUserExtended(n, a)
//}
