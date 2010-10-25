package com.readclosely.model
/*
 Copyright 2009-2010 Karl Pichotta

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._

import com.readclosely.util._

class DeletedMod extends LongKeyedMapper[DeletedMod] with IdPK{
    def getSingleton = DeletedMod // what's the "meta" server

    object modID extends MappedLong(this)
    object deletionDateTimeMillis extends MappedLong(this)

    object userID extends MappedLongForeignKey(this, User) {
        override def  dbIndexed_? = false
    }

    object passageID extends MappedLongForeignKey(this, Passage) {
        override def  dbIndexed_? = false
    }
    object sentenceID extends MappedInt(this)
    object annID extends MappedLongForeignKey(this, Annotation) {
        override def  dbIndexed_? = false
    }

    object isUp extends MappedBoolean(this)
}

/**
 * The singleton that has methods for accessing the database
 */
object DeletedMod extends DeletedMod with LongKeyedMetaMapper[DeletedMod] {
}

