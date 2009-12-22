package com.osinka.mongodb

import com.mongodb.ObjectId

trait MongoObject {
    var mongoOID: Option[ObjectId] = None
    var mongoNS: Option[String] = None
}