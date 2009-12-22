package com.osinka.mongodb

import com.osinka.mongodb._

object Helper {
    def fillWith[T](coll: MongoCollection[T], n: Int)(factory: (Int => T)) {
        Array.fromFunction(factory)(n) foreach { coll << _ }
    }
}
