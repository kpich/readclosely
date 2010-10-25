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

class Docs {

    def doctitle(xhtml: NodeSeq): NodeSeq = S.param("id") match {
        case Full(docID) => {
            try {
                Text(Doc.findAll(By(Doc.id, BasicTypesHelpers.asLong(docID).open_!))(0).title.is)
            } catch {
                case e: Exception => {
                    Log.warn("Couldn't find passage with id " + docID.toString)
                    Text("")
                }
            }
        }
        case _ => {
            Log.warn("Error in looking up doc (parameter 'id' not present or well-formed)")
            Text("")
        }
    }

    def frontpgdoclist(xhtml: NodeSeq): NodeSeq = DocUtil.createFrontPgDocListNodeSeq

    def alldocslist(xhtml: NodeSeq): NodeSeq = DocUtil.createAllDocsList

    def sectionListing(xhtml: NodeSeq): NodeSeq = S.param("id") match {
        case Full(docID) => {
            val secs = getSections(BasicTypesHelpers.toLong(docID))
            <div id="seclistwrapper">
                <div class="docmainpgtitle">{doctitle(xhtml)}</div>
                <div id="docsectionlisting">{DocUtil.getSectionListingDiv(BasicTypesHelpers.toLong(docID))}</div>
            </div>
        }
        case _ => {
            Log.error("Error in looking up doc (parameter 'id' not present or well-formed)")
            <div id="psgwrap"> no sections found for document :( </div>
        }
    }

    private def getSections(docID: Long): List[Section] = Section.findAll(By(Section.doc, docID))

}

