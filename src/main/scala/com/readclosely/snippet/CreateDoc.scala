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

class CreateText extends StatefulSnippet {
    var dispatch: DispatchIt = {
        case "newtext" => initialCreate _
    }

    var title = ""
    var author = ""
    var commaDelimitedPsgIDs = ""
    var doc: Doc = Doc.create

    private def initialCreate(in: NodeSeq): NodeSeq = User.currentUser match {
        case Full(user) => {

            bind("doc", in,
                "preview" -> Text(""),
                "title" -> SHtml.text(title, title=_, ("style", "width:70%")),
                "author" -> SHtml.text(author, author=_, ("style", "width:50%")),
                "textarea" -> SHtml.textarea(commaDelimitedPsgIDs, commaDelimitedPsgIDs = _, ("style", "width:100%"), ("rows", "26")),
                "submitDoc" -> SHtml.submit("Submit", () => submitDoc),
                )
        }
        case _ => Text("Please log in to add a new passage.")
    }

    private def submitDoc(): NodeSeq = {
        Log.info("in submit doc.")
        val doc = Doc.create.title(title).author(author).synopsis("A delightful something or other")
        doc.save //because otherwise it has a null id....
        var i: Int = 1
        (commaDelimitedPsgIDs.split(',')
            foreach
            (x => {Section.create.doc(doc.id.is).passage(BasicTypesHelpers.toLong(x)).sectionNum(i).save
                   i += 1}))
        doc.numSections(i-1).save
        <div>dummy</div>
    }

}
