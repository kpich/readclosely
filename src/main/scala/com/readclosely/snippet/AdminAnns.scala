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

import org.joda.time._

class AdminAnns {

    def adminAnnsList(xhtml: NodeSeq): NodeSeq =
        if(User.superUser_?) constructAdminAnnsList
        else <div> Sorry, you must be an admin to use this. </div>

    private def constructAdminAnnsList(): Node =
        <div class="adminannslist">
            <h1>New Annotations</h1><br />
            {getPageOfNewAnns map (makeIntoAnnListDiv _)}
            <hr /> <hr /> <hr /> <hr /> <hr />
            <h1>Updated Annotations</h1><br />
            {getPageOfUpdatedAnns map (makeIntoAnnListDiv _)}
            <hr /> <hr /> <hr /> <hr /> <hr />
            <h1>Deleted Annotations</h1><br />
            {getPageOfDeletedAnns map (makeIntoDeletedAnnListDiv _)}
            <hr /> <hr /> <hr /> <hr /> <hr />
            <div>{getPrevLink}&nbsp;{getNextLink}</div>
        </div>

    private def getPageOfNewAnns(): List[Annotation] =
        Annotation.findAll(OrderBy(Annotation.submissionDateTimeMillis, Descending),
                           StartAt(pageSize * (pgNum - 1)),
                           MaxRows(pageSize))

    private def getPageOfUpdatedAnns(): List[Annotation] =
        Annotation.findAll(OrderBy(Annotation.lastEditDateTimeMillis, Descending),
                           StartAt(pageSize * (pgNum - 1)),
                           MaxRows(pageSize))

    private def getPageOfDeletedAnns(): List[DeletedAnnotation] =
        DeletedAnnotation.findAll(OrderBy(DeletedAnnotation.deletionDateTimeMillis, Descending),
                           StartAt(pageSize * (pgNum - 1)),
                           MaxRows(pageSize))

    private def pgNum = BasicTypesHelpers.toInt(S.param("p") openOr "1")

    private def makeIntoAnnListDiv(ann: Annotation) = 
        <div>
            <div><h2>{ann.commentText.is}</h2></div>
            <div>{
                User.find(By(User.id, ann.authorID.is)) match {
                    case Full(user) => user.name.is
                    case _ => "(unknown user (!))"
                }}
            </div>
            <div>{AnnotationUtil.getTimeAgoStr(ann)}</div>
            {getDeleteAnnA(ann)}
            <br /> <br /> <br />
        </div>

    private def getDeleteAnnA(ann: Annotation) =
        SHtml.a(
            () => {
                copyToDeletedAnn(ann)
                copyAnnModsToDeletedMods(ann)
                if(ann.delete_!) Alert("deleted (and copied to DeletedAnn)")
                else Alert("could not delete annotation (still probably copied to DeletedAnn though)")
            },
            Text("delete ann")
        )

    private def makeIntoDeletedAnnListDiv(ann: DeletedAnnotation) = 
        <div>
            <div><h2>{ann.commentText.is}</h2></div>
            <div>{
                User.find(By(User.id, ann.authorID.is)) match {
                    case Full(user) => user.name.is
                    case _ => "(unknown user (!))"
                }}
            </div>
            <div>{getDeletedAnnTimeAgoStr(ann)}</div>
            {getUndeleteAnnA(ann)}
            <br /> <br /> <br />
        </div>

    private def getUndeleteAnnA(ann: DeletedAnnotation) =
        SHtml.a(
            () => {
                copyToUndeletedAnnAndHandleMods(ann)
                if(ann.delete_!) Alert("deleted (and copied to Annotation object)")
                else Alert("could not delete annotation (still probably copied to Annotation though)")
            },
            Text("undelete ann")
        )

    private def getDeletedAnnTimeAgoStr(ann: DeletedAnnotation) = {
        val diffInSecs =(new DateTime().getMillis - ann.submissionDateTimeMillis.is) / 1000
            ((if(diffInSecs <= 60) "about " + diffInSecs + " second" + (if(diffInSecs!=1) "s" else "")
            else if(diffInSecs <= 3600) 
                "about " + (diffInSecs/60) + " minute"+ (if(diffInSecs/60!=1) "s" else "")
            else if(diffInSecs <= 86400) 
                "about " + (diffInSecs/3600) + " hour"+ (if(diffInSecs/3600!=1) "s" else "")
            else if(diffInSecs <= 2687040) 
                "about " + (diffInSecs/86400) + " day"+ (if(diffInSecs/86400!=1) "s" else "")
            else if(diffInSecs <= 31553280) 
                "about " + (diffInSecs/2635200) + " month"+ (if(diffInSecs/2635200!=1) "s" else "")
            else "over " + (diffInSecs/31553280) + " year"+ (if(diffInSecs/31553280!=1) "s" else ""))
        + " ago")
    }

    private def getPrevLink = <a href={"/adminAnns.html?p=" + (pgNum - 1)}>Prev</a>
    private def getNextLink = <a href={"/adminAnns.html?p=" + (pgNum + 1)}>Next</a>


    private def copyToDeletedAnn(ann: Annotation) = {
        val d: DeletedAnnotation = DeletedAnnotation.create
                .annid(ann.id.is)
                .deletionDateTimeMillis(new DateTime().getMillis)
                .score(ann.score.is)
                .commentText(ann.commentText.is)
                .authorID(ann.authorID.is)
                .passageID(ann.passageID.is)
                .sentenceID(ann.sentenceID.is)
                .submissionDateTimeMillis(ann.submissionDateTimeMillis.is)
                .lastEditDateTimeMillis(ann.lastEditDateTimeMillis.is)
                .inReplyTo(ann.inReplyTo.is)
                .parentAnn(ann.parentAnn.is)
                .replyOrder(ann.replyOrder.is)
                .numReplies(ann.numReplies.is)
        d.save
    }

    private def copyToUndeletedAnnAndHandleMods(ann: DeletedAnnotation) = {
        val a: Annotation = new Annotation()
        a.score(ann.score.is)
            .commentText(ann.commentText.is)
            .authorID(ann.authorID.is)
            .passageID(ann.passageID.is)
            .sentenceID(ann.sentenceID.is)
            .submissionDateTimeMillis(ann.submissionDateTimeMillis.is)
            .lastEditDateTimeMillis(ann.lastEditDateTimeMillis.is)
            .inReplyTo(ann.inReplyTo.is)
            .parentAnn(ann.parentAnn.is)
            .replyOrder(ann.replyOrder.is)
            .numReplies(ann.numReplies.is)
        a.save
        changeReferencesToUndeletedAnnAcrossDB(ann.annid.is, a.id.is)
    }

    private def changeReferencesToUndeletedAnnAcrossDB(oldID: Long, newID: Long) = {
        Annotation.findAll(By(Annotation.inReplyTo, oldID)) map( x => x.inReplyTo(newID).save )
        Annotation.findAll(By(Annotation.parentAnn, oldID)) map( x => x.parentAnn(newID).save )
        undeleteMods(oldID, newID)
    }

    private def undeleteMods(oldAnnID: Long, newAnnID: Long) = {
        DeletedMod.findAll(By(DeletedMod.modID, oldAnnID)) map ( x => 
            Mod.create
               .userID(x.userID.is)
               .passageID(x.passageID.is)
               .sentenceID(x.sentenceID.is)
               .annID(newAnnID)
               .isUp(x.isUp.is)
               .save
        )
    }

    private def copyAnnModsToDeletedMods(ann: Annotation) =  {
        var sucCtr = 0
        var failCtr = 0
        Mod.findAll(By(Mod.annID, ann.id.is)).map(x => {
            DeletedMod.create
                .modID(x.id.is)
                .deletionDateTimeMillis(new DateTime().getMillis)
                .userID(x.userID.is)
                .passageID(x.passageID.is)
                .sentenceID(x.sentenceID.is)
                .annID(x.annID.is)
                .isUp(x.isUp.is)
            .save
            if(x.delete_!) sucCtr += 1 else failCtr += 1
        })
        Log.info("" + sucCtr + " Mods successfully deleted; " + failCtr + " unsuccessful Mod deletions.")
    }


    private val pageSize = 50
}
