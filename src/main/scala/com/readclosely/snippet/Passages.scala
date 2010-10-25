package com.readclosely.snippet
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
import S._ 
import _root_.net.liftweb.util._ 
import _root_.net.liftweb.mapper._
import Helpers._ 

import scala.xml._
import com.readclosely.model._
import com.readclosely.util._

class Passages {

    def passagetext(xhtml: NodeSeq): NodeSeq = S.param("id") match {
        case Full(psgID) => {
            try {
                PassageUtil.createPassageNodeSeq(
                    Passage.findAll(By(Passage.id, BasicTypesHelpers.asLong(psgID).open_!))(0))
            } catch {
                case e: Exception => {
                    Log.warn("Couldn't find passage with id " + psgID.toString)
                    Text("Error in processing :(")
                    }
            }
        }
        case _ => {
            Log.warn("Error in looking up passage (parameter 'id' not present or well-formed)")
            Text("Text not found :(")
        }
    }

    def passagetitle(xhtml: NodeSeq): NodeSeq = S.param("id") match {
        case Full(psgID) => {
            try {
                Text(Passage.findAll(By(Passage.id, BasicTypesHelpers.toLong(psgID)))(0).title.is)
            } catch {
                case e: Exception => {
                    Log.warn("Couldn't find passage with id " + psgID.toString)
                    Text("")
                }
            }
        }
        case _ => {
            Log.warn("Error in looking up passage (parameter 'id' not present or well-formed)")
            Text("")
        }
    }

    def viewPassagePageTitle(xhtml: NodeSeq): NodeSeq = S.param("id") match {
        case Full(psgID) => {
            try {
                val psgTitle = Passage.findAll(By(Passage.id, BasicTypesHelpers.toLong(psgID)))(0).title.is
                val docTitle = getDocTitle(BasicTypesHelpers.toLong(psgID))
                if(docTitle == null) Text("ReadClosely :: "+psgTitle)
                else Text("ReadClosely :: "+docTitle + " :: "+ psgTitle)
            } catch {
                case e: Exception => {
                    Log.error("couldn't find passage wth id "+psgID)
                    Text("")
                }
            }
        }
        case _ => {
            Log.warn("Error in looking up passage (parameter 'id' not present or well-formed)")
            Text("")
        }
    }

    //@TODO this is just horrible w.r.t. DB lookups. Maybe do something about this.
    private def getDocTitle(psgID: Long): String = {
        try {
            val sec = Section.findAll(By(Section.passage, psgID)).head
            if(Section.findAll(By(Section.doc, sec.doc.is)).length <= 1) null
            else Doc.findAll(By(Doc.id, sec.doc.is)).head.title.is
        } catch {
            case e: Exception => {
                Log.error("couldn't find title for passage with ID " + psgID.toString)
                null
            }
        }
    }


    def stubpassagelist(xhtml: NodeSeq): NodeSeq = 
    {
        PassageUtil.createPassageListNodeSeq(
            Passage.findAll.map(x => x.title.is)
            zip
            Passage.findAll.map(x => x.id.is)
        )
    }

}

