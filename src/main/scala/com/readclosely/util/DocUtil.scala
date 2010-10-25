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

import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._

import readclosely.model._
import readclosely.util._
import readclosely.snippet._
import scala.xml._


object DocUtil
{

    def createFrontPgDocListNodeSeq(): Node =
        <div class="frontdoclist">
            {Doc.findAll(StartAt(0), MaxRows(20)).map(makeFrontDocListing _)}
            <a href="/docs">
            <div class="seealldocs">
                See all...
            </div>
            </a>
        </div>

    def createAllDocsList(): Node =
        <div class="alldocslist">
            {Doc.findAll(OrderBy(Doc.title, Ascending)).map(makeFrontDocListing _)}
        </div>

    def makeFrontDocListing(doc: Doc): Node = 
        <a href={getDocUrl(doc)}>
            <div class="frontdoc">
                <div class="frontdoctitle">{getDocTitle(doc)}</div>
                <div class="frontdocbyline">
                    {getDocAuthorSpan(doc)}
                    <span class="frontdocanncount">{getAnnotationCount(doc)}</span> annotations.
                </div>
            </div>
        </a>

    def getDocUrl(doc: Doc): String = "doc/" + doc.id.is

    def getDocTitle(doc: Doc) = doc.title.is

    def getDocTitle(docID: Long) = Doc.find(By(Doc.id, docID)) match {
        case Full(doc) => doc.title.is
        case _ => ""
    }

    def getSecTitle(docID: Long, secNum: Int) =
        Section.find(By(Section.doc, docID), By(Section.sectionNum, secNum)) match {
            case Full(sec) => Passage.find(By(Passage.id, sec.passage.is)) match {
                case Full(psg) => psg.title.is
                case _ => ""
            }
            case _ => ""
        }

    def getSectionListingDiv(docID: Long): NodeSeq = 
        interpolatePsgNodesAndSubTitles(
            getSectionPassageList(docID).map( x => (constructSectionDisplay(x._1, x._2, docID), x._2) ),
            getSubTitles(docID))

    private def getDocAuthorSpan(doc: Doc) =
        if(doc.author.is != null && doc.author.is != "")
            <span class="frontdocauthor">by&nbsp;{doc.author.is}</span>
        else
            <span class="empauth"/>

    private def getSectionPassageList(docID: Long): List[(Passage, Int)] = 
        ((Section.findAll(By(Section.doc, docID), OrderBy(Section.sectionNum, Ascending))
         .map
          (x => (x.passage.is, x.sectionNum.is)))
           .map
            (y => ( Passage.findAll(By(Passage.id, y._1))(0),
                    y._2
                  )
            )
        )

    private def getSubTitles(docID: Long) =
        DocSubTitle.findAll(By(DocSubTitle.doc, docID),
                            OrderBy(DocSubTitle.sectionNumImmediatelyFollowing, Ascending))

    private def interpolatePsgNodesAndSubTitles(psgNodesAndSecNums: List[(Node, Int)],
                                                subs: List[DocSubTitle]) =  {
        //very unscletic. Unscalatic? Unscalar?
        var result_rev: List[Node] = Nil
        var curSubs = subs
        psgNodesAndSecNums.foreach(x => {
            if(curSubs.isEmpty)
                result_rev = x._1 :: result_rev
            else if(curSubs.head.sectionNumImmediatelyFollowing.is == x._2) {
                result_rev = x._1 :: constructSubTitleNode(curSubs.head) :: result_rev
                curSubs = curSubs.tail
            }
            else result_rev = x._1 :: result_rev
        })
        result_rev.reverse
    }

    private def constructSubTitleNode(st: DocSubTitle): Node = 
        <div class="docsubtitle">{st.text.is}</div>

    def getAnnotationCount(doc: Doc): Int = 
        ((Section.findAll(By(Section.doc, doc.id.is))
          .map(x => PassageUtil.getPassageAnnotationCount(x.passage.is))
         )
         .foldLeft(0){(x, y) => BasicTypesHelpers.toInt(x) + BasicTypesHelpers.toInt(y)}
        )

    private def constructSectionDisplay(psg: Passage, secSeqNum: Int, docID: Long): Node =
        <div class="secLink">
            {constructSectionAnchor(psg, secSeqNum: Int, docID)}
        </div>

    private def constructSectionAnchor(psg: Passage, secSeqNum: Int, docID: Long): Node =
        <a href={"/doc/" + docID + "/" + secSeqNum}>{constructSectionAnchorText(psg)}</a>

    private def constructSectionAnchorText(psg: Passage): Node =
        <div class="secLinkTxt">
            <div class="secTitle">
                <span class="secTitleText">{Unparsed(psg.title.is)}</span>
                <span class="secAnnCount">{getSecAnnCountStr(psg)}</span>
            </div>
            <div class="secPrevTxt">
                {Unparsed(constructSectionPrevText(psg))}
            </div>
        </div>

    private def getSecAnnCountStr(psg: Passage): String =
        "(" + PassageUtil.getPassageAnnotationCount(psg) + ")"

    private def constructSectionPrevText(psg: Passage): String = 
        removeHtml(
            if(psg.synopsis.is != null && psg.synopsis.is != "")
                psg.synopsis.is
            else if(psg.sentences.is.length <= PREV_LEN)
                psg.sentences.is
            else
                getTrimmedPrevText(psg) + "..."
        )

    private def getTrimmedPrevText(psg: Passage) = {
        val trimmed = psg.sentences.is.substring(0, PREV_LEN)
        val spInd = trimmed.lastIndexOf(" ")
        val amInd = trimmed.lastIndexOf("&")
        if(amInd > spInd) trimmed.substring(0, amInd) else trimmed
    }

    private def removeHtml(in: String): String =
        in.replaceAll("(<\\S*>|</\\S*>)", "").replaceAll("(<\\S*|</\\S*)", "")

    private val PREV_LEN = 300
}
