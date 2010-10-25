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
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js._
import Helpers._ 

import scala.xml._
import com.readclosely.model._
import com.readclosely.util._


import org.joda.time._


class Annotations {

    def render:Node = S.param("id") match {
        case Full(psgID) =>
            try {
               <div id="anncontainer">
                    {AnnotationUtil.getAnnotations(BasicTypesHelpers.asLong(psgID).open_!)}
               </div>
            } catch {
                //since we're calling S.Redirect within the try block abovce, we don't want to catch
                //ResponseShortcutExceptions.
                case e: Exception if(!e.isInstanceOf[net.liftweb.http.ResponseShortcutException]) =>  {
                    Log.warn("Exception in looking up initial annotations (parameter 'id' not present or well-formed)")
                    Text("No annotations Found")
                }
            }
        case _ => {
            Log.warn("Error in looking up initial annotations (parameter 'id' not present or well-formed)")
            Text("No annotations Found")
        }
    }



    def stubannotationform(xhtml: NodeSeq): NodeSeq = 
    {
        println(xhtml.toString)
        val a = Annotation.create
        var myAnnotation = ""

        /**
        *@return passageID value of relevant passage, -1 if a problem.
        */
        def getPsgID():Long = 
        {
            try {
                var psgId: Long = -1
                val it = xhtml.elements
                while(it.hasNext) {
                    val possPsgID = (it.next \ "@passageID").text
                    if(possPsgID != null) psgId = BasicTypesHelpers.asLong(possPsgID) match {
                        case Full(id) => id
                        case _ => psgId
                    }
                }
                psgId
            }
            catch {
                case e: Exception => {println(e.toString);-1}
            }
        }

        /**
        *@return sentID value of relevant sentence, -1 if a problem.
        */
        def getSentID(): Int =
        {
            try {
                var sentId = -1
                val it = xhtml.elements
                while(it.hasNext) {
                    val possSentID = (it.next \ "@sentID").text
                    if(possSentID != null) sentId = BasicTypesHelpers.asInt(possSentID) match {
                        case Full(id) => id
                        case _ => sentId
                    }
                }
                sentId
            }
            catch {
                case e: Exception => {println(e.toString);-1}
            }
        }

        def addNewAnnotation() = 
            new Annotation().commentText(myAnnotation).score(1).authorID(AnnotationUtil.getCurUser).passageID(getPsgID).sentenceID(getSentID).save

        def makeSubmitJSCmd() = {
            () => { 
                addNewAnnotation()
            }
        }

        bind("a", xhtml,
              "annTextarea" -> SHtml.textarea("", myAnnotation = _), 
              "submitAnn" -> SHtml.submit("annotate!", makeSubmitJSCmd)
            )
    }


}

object AnnOrdering extends SessionVar[AnnotationUtil.Order](AnnotationUtil.Conversation)
