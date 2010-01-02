package com.osinka.mongodb

import com.osinka.mongodb._

object Helper {
    def fillWith[T : Manifest](coll: MongoCollection[T], n: Int)(factory: (Int => T)) {
        Array.tabulate(n)(factory) foreach { coll << _ }
    }
}
