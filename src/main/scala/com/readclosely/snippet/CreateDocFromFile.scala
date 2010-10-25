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
import net.liftweb.http.js.JsCmds._

import scala.io._
import scala.xml._
import com.readclosely.model._
import com.readclosely.util._
import java.net._

class CreateDocFromFile extends StatefulSnippet {
    var dispatch: DispatchIt = {
        case "newdoc" => initialCreate _
    }

    var title = ""
    var author = ""
    //list of (title, synopsis, startNum, psg) tuples:
    var secs: List[(String, String, Int, String)] = Nil
    //list of (subtitle, nextSecNum) tuples:
    var subtitles: List[(String, Int)] = Nil
    var secsNode: Node = <div/>
    var submitButton: Node = <div />
    var fileHolder : Box[FileParamHolder] = Empty

    private def initialCreate(in: NodeSeq): NodeSeq = User.currentUser match {
        case Full(user) => {

            bind("newdoc", in,
                "previewsecs" -> secsNode,
                "title" -> Text(title),
                "author" -> Text(author),
                "upload" -> SHtml.fileUpload(x => fileHolder = Full(x)),
                "submit" -> SHtml.submit("Show Preview", () => processFile),
                "submitdoc" -> submitButton
                )
        }
        case _ => Text("Please log in to add a new passage.")
    }


    private def processFile() = fileHolder match {
        case Full(FileParamHolder(_, _, _, file)) => {
            var lines: List[String] = Nil 
            Source.fromBytes(file).getLines.foreach (x => lines = x :: lines)
            lines = lines.reverse
            //the assumption is that <title>X</title> is the first line
            title = extractTitle(lines.head)
            //we also assume that <author>X</author> is the second line
            author = extractAuthor(lines.tail.head)
            secs = extractSections(lines.tail.tail)
            subtitles = extractSubtitles(lines.tail.tail)
            secsNode = makeSecsNode
            submitButton = SHtml.submit("submit doc!", () => submitDocToDB)
        }
        case _ => {
            Log.error("in CreateDocFromFile.processFile, fileHolder was empty.")
        }
    }

    private def submitDocToDB() = {
        val psgs = secs.map(x =>
            {Passage.create.sentences(x._4)
                           .title(x._1)
                           .numSentences(countNumSentences(x._4))
                           .firstDisplayedSentNum(x._3)
                           .synopsis(if(x._2 == "" ) null else x._2)
        })
        psgs.foreach(_.save)
        val doc = Doc.create.title(title)
                            .author(if(author == "") null else author)
                            .numSections(psgs.length)
        doc.save
        val sections = psgs.map(x => Section.create.doc(doc.id.is).passage(x.id.is))
        var i = 1
        sections.foreach(x => {x.sectionNum(i)
                               i += 1})
        sections.foreach(_.save)
        subtitles.foreach(x =>
            DocSubTitle.create
            .doc(doc.id.is)
            .sectionNumImmediatelyFollowing(x._2)
            .text(x._1)
            .save)
    }

    private def countNumSentences(psgTxt: String): Int =
        PassageUtil.splitIntoSentences(psgTxt).remove(_.trim == "").length

    private def extractTitle(titleLine: String): String = 
        titleLine.replaceAll("<title>", "").replaceAll("</title>", "").trim

    private def extractAuthor(authorLine: String): String = 
        authorLine.replaceAll("<author>", "").replaceAll("</author>", "").trim

    private def makeSecsNode: Node =
        <div class="prevseclist">
            {secs.map(x => makePrevSec(x._1, x._4))}
        </div>

        private def makePrevSec(title: String, psg: String): Node =
            <div class="prevsec">
                <div class="prevsectitle">
                    {title}
                </div>
                <table class="prevsecpsg">
                    {(psg.split("\n").map(x => <tr>{Unparsed(x)}</tr>))}
                </table>
            </div>

    /**
        Note this doesn't work with actual XML, but instead a gimpy version that isn't whitespace
        invariant. Will return "" for synopsis if  none present.
        returns (title, synopsis, sentNumOffset, psg) tuples
    */
    private def extractSections(lines: List[String]): List[(String, String, Int, String)] = {
        var secList: List[(String, String, Int, String)] = Nil
        var curSec = ""
        var curTitle = ""
        var curSyn = ""
        var curFirstLineNum = 1
        lines foreach ({cur =>
            (if(cur.indexOf("<section") != -1) {
                curTitle = extractSecTitle(cur)
                Log.info(curTitle)
                curFirstLineNum = extractSecFirstLineNum(cur)
                curSyn = extractSynopsis(cur)
             }
             else if(cur.indexOf("</section>") != -1) { 
                secList = (curTitle, curSyn, curFirstLineNum, curSec) :: secList
                curTitle = ""
                curSec = ""
                curFirstLineNum = 1
             } else if(cur.indexOf("<subtitle") == -1) curSec += cur
               else ()
            )})
        secList.reverse
    }

    /*
        returns (subtitleText, secNumOfNextSec) tuples
    */
    private def extractSubtitles(lines: List[String]): List[(String, Int)] = {
        var numSecsEncountered = 0
        var res_rev: List[(String, Int)] = Nil
        lines foreach (cur => {
            if(cur.indexOf("<section") != -1)
                numSecsEncountered += 1
            if(cur.indexOf("<subtitle") != -1)
                res_rev = (extractSubtitleText(cur), numSecsEncountered + 1) :: res_rev
        })
        res_rev.reverse
    }

    private def extractSecTitle(sectionHeadLine: String): String = 
        extractAttributeFromSectionLine(sectionHeadLine, "name")

    private def extractSecFirstLineNum(sectionHeadLine: String): Int =  
        if(extractAttributeFromSectionLine(sectionHeadLine, "start") == "") 1
        else BasicTypesHelpers.toInt(
            extractAttributeFromSectionLine(sectionHeadLine, "start"))

    private def extractSynopsis(sectionHeadLine: String): String =  
        extractAttributeFromSectionLine(sectionHeadLine, "synopsis")

    private def extractAttributeFromSectionLine(sectionHeadLine: String, att: String) = 
        if(sectionHeadLine.indexOf(att + "=") == -1) ""
        else sectionHeadLine.substring(
                sectionHeadLine.indexOf(att +"=") + att.length + 2,
                sectionHeadLine.indexOf("\"", sectionHeadLine.indexOf(att + "=") + att.length + 3))

    private def extractSubtitleText(line: String) =
        line.substring(line.indexOf("text=") + 6,
                       line.indexOf("\"", line.indexOf("text=") + 7))


}
