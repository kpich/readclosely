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

import scala.xml._
import com.readclosely.model._
import com.readclosely.util._

class CreatePassage extends StatefulSnippet {
    var dispatch: DispatchIt = {
        case "newpassage" => initialCreate _
    }

    var title = ""
    var psgText = ""
    var warningText = ""
    var psg: Passage = null

    private def initialCreate(in: NodeSeq): NodeSeq = User.currentUser match {
        case Full(user) => {
            def previewPassage() = {
                if(title.trim == "") warningText = "You must provide a Title"
                else if(psgText.trim == "") warningText = "You must provide a passage"
                else {
                    warningText = "This is a PREVIEW. You must still click the \"Submit Passage\" button"
                    dispatch = {case "newpassage" => previewCreate _}
                }
            }

            bind("psg", in,
                "warning" -> <div id="warning">{warningText}</div>,
                "preview" -> Text(""),
                "title" -> SHtml.text(title, title=_, ("style", "width:70%")),
                "textarea" -> SHtml.textarea(psgText, psgText = _, ("style", "width:100%"), ("rows", "26")),
                "previewPsg" -> SHtml.submit("Show Preview", () => previewPassage),
                "submitPsg" -> Text("")
                )
        }
        case _ => Text("Please log in to add a new passage.")
    }

    private def previewCreate(in: NodeSeq): NodeSeq =
    {
            def previewPassage() = {
                if(title.trim == "") warningText = "You must provide a Title"
                else if(psgText.trim == "") warningText = "You must provide a passage"
                else {
                    warningText = "This is a PREVIEW. You must still click the \"Submit Passage\" button"
                    dispatch = {case "newpassage" => previewCreate _}
                }
            }

            def submitPassage = {
                if(title.trim == "") warningText = "You must provide a Title"
                else if(psgText.trim == "") warningText = "You must provide a passage"
                else {
                    //since psg is created, we repopulate it, save it, redirect to it.
                    psg.title(title.trim)
                       //Delimit by " \n" rather than "\n" because I think List.FromString("\n\n",'\n') has one elem.
                       .sentences(PassageUtil.splitIntoSentences(psgText).reduceLeft((x,y)=> x + "\n" + y))
                       .numSentences(PassageUtil.splitIntoSentences(psgText).remove(_.trim == "").length)
                       .save
                    unregisterThisSnippet
                    S.redirectTo("passage/" + psg.id.is)
                }
            }

            //@TODO change this fn so it doesn't eat a passage ID each time.
            def constructPreviewDiv = {
                psg = Passage.create.title(title.trim)
                                    .sentences(psgText)
               <div>[redacted]</div>
            }

            bind("psg", in,
                "warning" -> <div id="warning">{warningText}</div>,
                "preview" -> constructPreviewDiv,
                "title" -> SHtml.text(title, title=_, ("style", "width:70%")),
                "textarea" -> SHtml.textarea(psgText, psgText = _, ("style", "width:100%"), ("rows", "26")),
                "previewPsg" -> SHtml.submit("Show Preview", () => previewPassage),
                "submitPsg" -> SHtml.submit("Submit Passage", () => submitPassage)
                )
    }


}
