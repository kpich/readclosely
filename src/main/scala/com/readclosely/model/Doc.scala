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

class Doc extends LongKeyedMapper[Doc] with IdPK{
    def getSingleton = Doc // what's the "meta" server

    object title extends MappedString(this, Doc.TITLE_MAX_LEN)
    object author extends MappedString(this, Doc.AUTHOR_MAX_LEN)
    object synopsis extends MappedText(this)
    object numSections extends MappedInt(this)
}

object Doc extends Doc with LongKeyedMetaMapper[Doc] {
    val TITLE_MAX_LEN = 200
    val AUTHOR_MAX_LEN = 100
}

