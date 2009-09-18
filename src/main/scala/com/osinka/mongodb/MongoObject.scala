package com.osinka.mongodb

import com.mongodb.ObjectId

trait MongoObject {
    var mongoOID: ObjectId = _
    var mongoNS: String = _
}