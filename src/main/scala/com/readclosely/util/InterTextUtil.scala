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
import scala.util.matching.Regex


object InterTextUtil
{
    def resolveIntertextualReferences(ann: List[Node]): List[Node] = {
        var res_rev: List[Node] = Nil
        ann foreach {x =>
            res_rev = splitAndResolveReferences(x).reverse ++ res_rev
        }
        res_rev.reverse
    }


    private def splitAndResolveReferences(n: Node): List[Node] = n match {
        case Text(text) => text match {
            case IntertextRefPattern3Nums(prestuff, docid, seqnum, linenum, poststuff) =>
                splitAndResolveReferences(Text(prestuff)) ++
                List(getIntertextA(docid, seqnum, linenum)) ++
                splitAndResolveReferences(Text(poststuff))
            case IntertextRefPattern2Nums(prestuff, docid, secondnum, poststuff) =>
                splitAndResolveReferences(Text(prestuff)) ++
                List(getIntertextA(docid, secondnum)) ++
                splitAndResolveReferences(Text(poststuff))
            case IntertextRefPattern1Num(prestuff, docid, poststuff) =>
                splitAndResolveReferences(Text(prestuff)) ++
                List(getIntertextA(docid)) ++
                splitAndResolveReferences(Text(poststuff))
            case _ => List(n)
        }
        case _ => List(n)
    }

    private def getIntertextA(docID: String, seqNum: String, lineNum: String) =
        <a href={getIntertextLink(docID, seqNum, lineNum)}>
            {getIntertextText(docID, seqNum, lineNum)}
        </a>

    private def getIntertextA(docID: String, secondNum: String) =
        <a href={getIntertextLink(docID, secondNum)}>
            {getIntertextText(docID, secondNum)}
        </a>

    private def getIntertextA(docID: String) =
        <a href={getIntertextLink(docID)}>
            {getIntertextText(docID)}
        </a>

    //@TODO verify format of docID, seqNum, lineNum, perhaps -- XSS vulnerability?
    private def getIntertextLink(docID: String, seqNum: String, lineNum: String) =
        "/doc/" + docID + "/" + seqNum + "#" + lineNum

    private def getIntertextText(docID: String, seqNum: String, lineNum: String) =
        DocUtil.getDocTitle(BasicTypesHelpers.toLong(docID)) + ", " +
        DocUtil.getSecTitle(BasicTypesHelpers.toLong(docID),
                            BasicTypesHelpers.toInt(seqNum)) +
        ", line " + lineNum

    //@TODO disambiguate between cases where secondNum is secNum and lineNum.
    private def getIntertextLink(docID: String, secondNum: String) =
        "/doc/" + docID + "/" + secondNum

    private def getIntertextText(docID: String, secondNum: String) =
        DocUtil.getDocTitle(BasicTypesHelpers.toLong(docID)) + ", " +
        DocUtil.getSecTitle(BasicTypesHelpers.toLong(docID),
                            BasicTypesHelpers.toInt(secondNum)) 

    private def getIntertextLink(docID: String) =
        "/doc/" + docID

    private def getIntertextText(docID: String) =
        DocUtil.getDocTitle(BasicTypesHelpers.toLong(docID))

    val IntertextRefPattern3Nums = """(.*)@([0-9]+):([0-9]+):([0-9]+)(.*)""".r
    val IntertextRefPattern2Nums = """(.*)@([0-9]+):([0-9]+)(.*)""".r
    val IntertextRefPattern1Num = """(.*)@([0-9]+)(.*)""".r
}
