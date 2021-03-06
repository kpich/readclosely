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

class DeletedAnnotation extends LongKeyedMapper[DeletedAnnotation] with IdPK{
    def getSingleton = DeletedAnnotation // what's the "meta" server

    object annid extends MappedLong(this)
    object deletionDateTimeMillis extends MappedLong(this)

    object commentText extends MappedTextarea(this, DeletedAnnotation.MAX_LEN) {
        override def textareaRows  = 10
        override def textareaCols = 50
    }

    object score extends MappedInt(this)

    object authorID extends MappedLongForeignKey(this, User) {
        override def dbIndexed_? = false
    }

    object passageID extends MappedLongForeignKey(this, Passage) {
        override def dbIndexed_? = false
    }
    object sentenceID extends MappedInt(this)

    //had to change since MappedDateTime sets time = 00:00:000
    //object submissionDatetime extends MappedDateTime(this)
    object submissionDateTimeMillis extends MappedLong(this)

    //may be null.
    object lastEditDateTimeMillis extends MappedLong(this)

    //@todo: add dependency
    object inReplyTo extends MappedLong(this) {
        override def dbIndexed_? = false
    }
    object parentAnn extends MappedLong(this)

    /**
    *Int dictating which element in a "conversation" list this annotation appears.
    *Everything that is not a reply to something else will have replyOrder 0;
    *the first reply to a comment will have replyOrder 1, ....
    */
    object replyOrder extends MappedInt(this) {
        override def defaultValue = 0
    }

    object numReplies extends MappedInt(this) {
        override def defaultValue = 0
    }

}

/**
 * The singleton that has methods for accessing the database
 */
object DeletedAnnotation extends DeletedAnnotation with LongKeyedMetaMapper[DeletedAnnotation] {
    val MAX_LEN = 2000
}

