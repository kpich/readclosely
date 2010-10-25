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


object PassageUtil
{

/**
*Takes string, splits based on \n's, returns list of strings. IMPORTANT: Will condense
*any consecutive string of whitespace lines to a single empty string. This
*empty string should indicate a Logical break in the text; it should not be commentable.
*/
def splitIntoSentences(psg: String): List[String] = {
    val arr = psg.split("\n").map(cleanLineWhitespace _)
    val lineToRemove = " "
    (for(i <- List.range(0, arr.length)) yield {
        if(arr(i).length != 0) arr(i)
        else if(i != 0 && arr(i-1).length != 0 && arr(i).length == 0) arr(i)
        else lineToRemove
     }
    ).remove(_ ==  lineToRemove)
}

/**
Takes a list of sentences and the passage ID, returns a NodeList of xhtml good for
displaying on the site.
*/
def createPassageNodeSeq(psg: Passage): Node = 
{
    <div id="psgwrap">
        {getPsgNavDiv(psg)}
	<table class="psgtext">
	    {getPsgTitleTR(psg)}
	    {getPsgBylineTR(psg)}
	    {getPsgTxtTRs(psg.sentences.is, psg.id.is)}
	</table>
        {getPsgNavDiv(psg)}
    </div>
}

def getPassageAnnotationCount(psg: Passage) =
    Annotation.count(By(Annotation.passageID, psg.id.is))

def getPassageAnnotationCount(psgID: Long) =
    Annotation.count(By(Annotation.passageID, psgID))

private def getPsgNavDiv(psg: Passage): Node = {
    //@TODO maybe modify Passage schema so we don't have to do this....
    val sec = Section.findAll(By(Section.passage, psg.id.is))
    val secs = if(sec.isEmpty) Nil else Section.findAll(By(Section.doc, sec.head.doc.is))
    <table class="psgnavdiv">
        <tr>
            <td class="psgnavprev"> {getPsgNavPrevAnchor(psg, secs)} </td>
            <td class="psgnavup"> {getPsgNavUpAnchor(psg, secs)} </td>
            <td class="psgnavnext"> {getPsgNavNextAnchor(psg, secs)} </td>
        </tr>
    </table>
}

private def getPsgNavPrevAnchor(psg: Passage, secs: List[Section]): Node = {
    val prevSec = getPrevSection(psg, secs)
    if(prevSec != null) <a href={"/doc/"+ prevSec.doc.is + "/" + prevSec.sectionNum.is}>&lt;&lt; back</a>
    else <div class="psgnavempty" />
}

private def getPsgNavUpAnchor(psg: Passage, secs: List[Section]): Node = {
    val docID = getDocID(psg, secs)
    if(docID != -1) <a href={"/doc/"+ docID}>^ up ^</a>
    else <div class="psgnavempty" />
}

private def getPsgNavNextAnchor(psg: Passage, secs: List[Section]): Node = {
    val nextSec = getNextSection(psg, secs)
    if(nextSec != null) <a href={"/doc/"+ nextSec.doc.is + "/" + nextSec.sectionNum.is}>next &gt;&gt;</a>
    else <div class="psgnavempty" />
}

/**
@return -1 if only one section exists (otherwise, returns ID of this passage's Doc.
*/
private def getDocID(psg: Passage, secs: List[Section]): Long = {
    val docID: Long = if(secs.isEmpty) -1 else secs.head.doc.is
    if(secs.length <= 1) -1
    else docID
}

/**
@return null iff no prev sec exists.
*/
private def getPrevSection(psg: Passage, secs: List[Section]): Section = 
    getNextSection(psg, secs.reverse)

/**
@return null if no next sec exists.
*/
private def getNextSection(psg: Passage, secs: List[Section]): Section = 
    //this is a goofy, goofy algorithm, but it's also an atypical task...
    if (secs.isEmpty) null
    else if (secs.head.passage.is == psg.id.is)
        if (secs.tail.isEmpty) null
        else secs.tail.head
    else getNextSection(psg, secs.tail)

/*
*Will return a div for the passage title. Note that it'll be a sentence link for sentence 0,
*which is reserved for the title.
*/
private def getPsgTitleTR(psg: Passage): Node =  {
    val annCount = AnnotationUtil.getAnnCountForSentence(psg.id.is, 0)
    <tr>
	<td /><td class="psgtitle sent" id={constructSentTDID(0)}>
            {createIndSentLink(Unparsed(psg.title.is), psg.id.is, 0, annCount == 0)}
        </td>
	{getAnnCountTd(psg.id.is, 0, annCount)}
    </tr>
}

private def getPsgBylineTR(psg: Passage): Node = 
    <tr class="byline">
	<td />{getBylineTD(psg)}<td />
    </tr>

private def getBylineTD(psg: Passage): Node = {
    val sec = try { Section.findAll(By(Section.passage, psg.id.is)).head } catch { case e: Exception => null }
    val doc = try { Doc.findAll(By(Doc.id, sec.doc.is)).head } catch { case e: Exception => null }
    if(doc == null) <td />
    else if(Section.findAll(By(Section.doc, doc.id.is)).length > 1)
            <td><span class="bylinetitle">from <i>{doc.title.is}</i></span>&nbsp;
                {getBylineAuthorSpan(doc)}</td>
    else
            <td>{getBylineAuthorSpan(doc)}</td>
}

private def getBylineAuthorSpan(doc: Doc) = 
    if(doc.author.is == null || doc.author.is == "") <span class="bylineemauth"/>
    else <span class="bylineauthor">by {doc.author.is}</span>

private def getPsgTxtTRs(psgText: String, psgID: Long): List[Node] = {
    var i = 0
    //we subtract one to account for the title:
    var sentNumOffset = Passage.find(By(Passage.id, psgID)) match {
        case Full(psg) => psg.firstDisplayedSentNum - 1
        case _ => {
            Log.error("Couldn't find psg of id " + psgID)
            0
        }
    }
    splitIntoSentences(psgText).map(x =>
        if(x.length == 0) <tr class="cr"/>
        else {i += 1; createIndSentTR(x, psgID, i, sentNumOffset)})
}

def createPassageListNodeSeq(titleIDTuples: List[(String, Long)]) = 
    Elem(null, "div", Null, TopScope, 
            (titleIDTuples.map(
              x => Elem(null, "a", 
               new UnprefixedAttribute( "href", GlobalConstants.BASE_URL + "passage/" + x._2.toString, Null), TopScope, 
                Text(if(x._1 == null) "(no title)" else x._1)
              )) zip 
              titleIDTuples.map(x => Elem(null, "br", Null, TopScope))
            ).flatten(y => List(y._1, y._2)): _*
        )

/**
*creates an  elem for an individual sentence.
*@param text the text to display
*@param psgID the id of the passage
*@param sentID the sentence ID; NOTE that 0 is reserved for the title
*/
private def createIndSentLink(displayedStuff: Node,
                              psgID: Long,
                              sentID: Int,
                              shouldOpenCommentLink: Boolean): Node = 
    SHtml.a(
        () => 
            Run("focusAnnsOnUnDrilledDownSent(" + sentID + ")") &
            (if(shouldOpenCommentLink) AnnotationUtil.activateBaseCommentLink(psgID, sentID) else Noop),
        displayedStuff,
        ("name", AnnotationUtil.getSentTextAnchorName(sentID))
    )


private def createIndSentTR(text: String,
                            psgID: Long,
                            sentID: Int,
                            displayedSentNumOffset: Int): Node = {
    val annCount = AnnotationUtil.getAnnCountForSentence(psgID, sentID)
    <tr><td class="linenum">{sentID + displayedSentNumOffset}</td>
        <td class="psgline sent" id={constructSentTDID(sentID)}>
            {createIndSentLink(Unparsed(text),psgID, sentID, annCount == 0)}
        </td>
	{getAnnCountTd(psgID, sentID, annCount)}
    </tr>
}

private def getAnnCountTd(psgID: Long, sentID: Int, annCount: Int): Node = {
        <td class={ "count" + (if(annCount == 0) " empty" else " nonempty")}>
            {createIndSentLink(Text("(" +annCount + ")"), 
                               psgID,
                               sentID,
                               annCount == 0)}
        </td>
}

private def cleanLineWhitespace(line: String): String = escapeInitialWhitespace(line).trim

private def escapeInitialWhitespace(line: String): String = 
    if(line.length == 0 || line.charAt(0) != ' ') line
    else "&nbsp;" + escapeInitialWhitespace(line.substring(1))


//Important because this convention is reproduced client-side so we have synched scrolling.
private def constructSentTDID(sentID: Int) = "sent"+sentID

}
