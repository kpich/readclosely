package com.readclosely.util
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

import _root_.net.liftweb.http._
import _root_.scala.xml._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._

import com.readclosely.model._

object SectionUtil
{

    /**
        This is a goofy little function. It will return a valid passage ID if and only if
        there is only one section in the document. It returns null if the number of sections
        in the doc isn't equal to one.
    */
    def getPsgIDOrNull(docID: String): String = {
        val secs = Section.findAll(By(Section.doc, BasicTypesHelpers.toLong(docID)))
        if(secs.length == 1) secs.head.passage.is.toString else null
    }

    def getPsgID(docID: String, secNum: String): String = {
        try {
            (Section.findAll(By(Section.doc, BasicTypesHelpers.toLong(docID)))
            .find(_.sectionNum.is == BasicTypesHelpers.toInt(secNum))) match {
                case Some(sec) => sec.passage.is.toString
                case None => {
                    Log.error("could not find a section with doc "+docID+" and sectionNum "+secNum)
                    "-1"
                }
            }
        } catch {
            case e: Exception => {
            Log.error("could not get passage id for docID "+docID+" and secNum "+secNum+".")
            "-1"
            }
        }
    }

}
