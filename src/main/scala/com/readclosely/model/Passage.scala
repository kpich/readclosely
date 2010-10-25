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

class Passage extends LongKeyedMapper[Passage] with IdPK{
    def getSingleton = Passage // what's the "meta" server

    /**
    *This should be a set of lines delimited by \n's, with logical breaks given by a \n\n sequence.
    */
    object sentences extends MappedText(this)

    object title extends MappedString(this, Passage.TITLE_MAX_LEN)

    object numSentences extends MappedInt(this)

    object synopsis extends MappedString(this, Passage.SYNOPSIS_MAX_LEN)

    //if we want a passage's sentence nums to start displaying at some n>1 we can do it here.
    object firstDisplayedSentNum extends MappedInt(this) {
        override def defaultValue = 1
    }

}

/**
 * The singleton that has methods for accessing the database
 */
object Passage extends Passage with LongKeyedMetaMapper[Passage] {
    val TITLE_MAX_LEN = 200
    val SYNOPSIS_MAX_LEN = 1000
}

