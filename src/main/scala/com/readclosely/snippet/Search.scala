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

class Search {

    def render(xhtml: NodeSeq): NodeSeq =  {
        def submitSearch(query: String) =
            S.redirectTo("search?q=" + urlEncode(query))

        bind("srch", xhtml,
             "desc" -> Text("Search for a text"),
             "text" -> SHtml.text("", x => submitSearch(x)),
             "submit" -> SHtml.submit("search", () => () ))
    }

    def results(xhtml: NodeSeq): NodeSeq = {
        <div id="searchresults">
            <div id="srchsubt">Search Results</div>
        {getResults}
        </div>
    }


    private def getResults(): NodeSeq = S.param("q") match {
        case Full(searchString) => getResultsDiv(urlDecode(searchString))
        case _ => Text("No results to show.")
    }


    private def getResultsDiv(searchString: String) = {
        val res = ( searchByDocTitle(searchString) union
                    searchByAuthor(searchString)   union
                    searchBySecTitle(searchString) )
        if(res.isEmpty) Text("No results to show.") else getNonemptyResults(res)
    }

    private def searchByDocTitle(searchString: String) =  {
        Doc.findAll(Like(Doc.title, "%" + searchString + "%")) map {
            x => DocData(x.title.is, x.author.is, DocUtil.getAnnotationCount(x), x.id.is)
        }
    }
    
    private def searchByAuthor(searchString: String) =  {
        Doc.findAll(Like(Doc.author, "%" + searchString + "%")) map {
            x => DocData(x.title.is, x.author.is, DocUtil.getAnnotationCount(x), x.id.is)
        }
    }

    private def searchBySecTitle(searchString: String) =  {
        Passage.findAll(Like(Passage.title, "%" + searchString + "%"))
        .remove(isOnlyPsgInDoc _)
        .map(x => PsgData(x.title.is,
                          getPsgAuthor(x),
                          BasicTypesHelpers.toInt(PassageUtil.getPassageAnnotationCount(x)),
                          getDocIDForPsg(x),
                          getSecNum(x)))
    }

    private def isOnlyPsgInDoc(psg: Passage) = try {
        val docID = getDocIDForPsg(psg)
        Section.findAll(By(Section.doc, docID)).length == 1
    } catch {
        case _ => {
            Log.error("in isOnlyPsgInDoc, error for psg " + psg.id.is)
            false
        }
    }

    def getSecNum(psg: Passage): Int = Section.find(By(Section.passage, psg.id.is)) match {
        case Full(sec) => sec.sectionNum.is
        case _ => {
            Log.error("in Search.getSecNum, couldn't find section for psg " + psg.id.is.toString)
            1
        }
    }

    def getDocIDForPsg(psg: Passage): Long = Section.find(By(Section.passage, psg.id.is)) match {
        case Full(sec) => sec.doc.is
        case _ => {
            Log.error("in Search.getDocIDForPsg, couldn't find section for psg " + psg.id.is.toString)
            1
        }
    }

    private def getPsgAuthor(psg: Passage) =
        Doc.findAll(By(Doc.id, getDocIDForPsg(psg))).head.author.is

    private def getNonemptyResults(results: List[PsgOrDocData]) = results.map(mkIndSearchResDiv _)
        
    private def mkIndSearchResDiv(data: PsgOrDocData) = data match {
        case PsgData(title, author, numAnns, docID, secNum) =>
            <div class="srchres">
                <div class="srchtitle">{getPsgTitleA(title, docID, secNum)}</div>
                <div class="srchauth">{author}</div>
                <div class="srchannct">{"(" + numAnns + " annotations)"}</div>
            </div>
        case DocData(title, author, numAnns, docID) =>
            <div class="srchres">
                <div class="srchtitle">{getDocTitleA(title, docID)}</div>
                <div class="srchauth">{author}</div>
                <div class="srchannct">{"(" + numAnns + " annotations)"}</div>
            </div>
    }

    private def getPsgTitleA(title: String, docID: Long, secNum: Int) =
        <a href={"/doc/" + docID.toString + "/" + secNum}>{title}</a>

    private def getDocTitleA(title: String, docID: Long) = 
        <a href={"/doc/" + docID.toString}>{title}</a>

        abstract class PsgOrDocData
        case class PsgData(title: String,
                           author: String,
                           numAnnotations: Int,
                           docID: Long,
                           secNum: Int) extends PsgOrDocData
        case class DocData(title: String,
                           author: String,
                           numAnnotations: Int,
                           docID: Long) extends PsgOrDocData
}

