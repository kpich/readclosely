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

class Mod extends LongKeyedMapper[Mod] with IdPK{
    def getSingleton = Mod // what's the "meta" server

    object userID extends MappedLongForeignKey(this, User)

    object passageID extends MappedLongForeignKey(this, Passage)
    object sentenceID extends MappedInt(this)
    object annID extends MappedLongForeignKey(this, Annotation)

    object isUp extends MappedBoolean(this)
}

/**
 * The singleton that has methods for accessing the database
 */
object Mod extends Mod with LongKeyedMetaMapper[Mod] {
    override def dbIndexes = Index(IndexField(passageID), IndexField(userID)) ::
                             Index(IndexField(passageID), IndexField(userID), IndexField(annID)) ::
                             Index(IndexField(passageID), IndexField(sentenceID), IndexField(userID)) ::
                             super.dbIndexes
}

