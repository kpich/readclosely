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

import _root_.net.liftweb.http._
import _root_.scala.xml._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._

import org.joda.time._

import com.readclosely.model._

object UserUtil
{

    def getUserProfileNode(username: String): Node =
        User.findAll(By(User.name, username)) match {
            case List(user) => constructUserProfileNode(user)
            case _ => Text("User not found.")
        }

    private def constructUserProfileNode(user: User): Node = 
        <div id="usrprofwrap">
            <div id="usranntitle">Annotatations Added by {user.name.is}</div>
            {Annotation.findAll(By(Annotation.authorID, user.id)).map(getIndUserAnnotationNode _)}
        </div>

        private def getIndUserAnnotationNode(ann: Annotation): Node =
            <div class="usrann">
                <div class="usrannbody">
                    {ann.commentText.is}
                    <div class="usranninfo">about {AnnotationUtil.getTimeAgoStr(ann)}, commenting on
                    {if(ann.sentenceID == 0) "title" else "line "+ann.sentenceID} of {getPsgTitleLink(ann)}</div>
                </div>
            </div>

            private def getPsgTitleLink(ann: Annotation): Node = Passage.find(By(Passage.id, ann.passageID.is)) match {
                case Full(psg) => <a href={"../../passage/"+psg.id.is}>{Unparsed(psg.title.is)}</a>
                case _ => Text("(title unknown)")
            }

}
