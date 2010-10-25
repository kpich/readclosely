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
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js._
import _root_.scala.xml._
import _root_.scala.collection.immutable.HashSet
import _root_.scala.collection.immutable.Set
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.StringHelpers._

import org.joda.time._

import com.readclosely.model._
import com.readclosely.snippet._

object AnnotationUtil extends HtmlFixer
{


    /**
    *@param psgID the passage ID
    *@param sentID the sentenceID (value 0 is reserved for the title)
    *@param ordering an instance of an Order class dictating order of listing.
    */
    def getAnnotations(psgID: Long): NodeSeq = 
        <div id="anns">
         <a name="AnnTop"/>
         {constructAnnotationListingDiv(psgID)}
        </div>

    /**
    *@return id of current user, -1 if user not logged in.
    */
    def getCurUser: Long = User.currentUser match {
        case Full(user) => user.id.is 
        case _ => {
            Log.info("Comment by user not logged in")
            -1
        }
    }


    def getAnnCountForSentence(psgID: Long, sentID: Int): Int = 
        BasicTypesHelpers.toInt(
            Annotation.count(By(Annotation.passageID, psgID), By(Annotation.sentenceID, sentID)))

    private def addNewBaseAnnotation(annotation: String, psgID: Long, sentID: Int): Annotation = {
        val curUser = getCurUser
        val ann =
            if(curUser != -1)
                (Annotation.create.commentText(annotation)
                                 .score(1)
                                 .authorID(curUser)
                                 .passageID(psgID)
                                 .sentenceID(sentID)
                                 .submissionDateTimeMillis((new DateTime()).getMillis))
            else
                (Annotation.create.commentText(annotation)
                                 .score(1)
                                 .passageID(psgID)
                                 .sentenceID(sentID)
                                 .submissionDateTimeMillis((new DateTime()).getMillis))
        ann.save
        ann.inReplyTo(ann.id.is).parentAnn(ann.id.is).save
        addAutoUpMod(ann)
        //silly. I'm aware. before save is called, id == -1, which is a problem
        ann
    }


    private def constructAnnotationListingDiv(psgID: Long): Node = {
    	val annLists: List[InitAnnList] = getInitAnnListFromDB(psgID)
        val usrMods: Set[Mod] = getUsrMods(psgID)
        val upMods: Set[Long] = getUpMods(usrMods)
        val downMods: Set[Long] = getDownMods(usrMods)
        val displayedSentNumOffset = getDisplayedSentNumOffset(psgID)
        <div class="annlistwrap">
            {getChangeViewDiv(psgID)}
            {constructAnnotationListing(psgID, annLists, upMods, downMods, displayedSentNumOffset)}
        </div>
    }


    private def getUsrMods(psgID: Long): Set[Mod] = User.currentUser match {
        case Full(user) => {
            HashSet[Mod]() ++ Mod.findAll(By(Mod.passageID, psgID), By(Mod.userID, user.id.is))
        }
        case _ => HashSet[Mod]()
    }
    private def getUsrModsForSent(psgID: Long, sentID: Int): Set[Mod] = User.currentUser match {
        case Full(user) => {
            HashSet[Mod]() ++ Mod.findAll(By(Mod.passageID, psgID),
                                          By(Mod.sentenceID, sentID),
                                          By(Mod.userID, user.id.is))
        }
        case _ => HashSet[Mod]()
    }

    private def getUpMods(usrMods: Set[Mod]): Set[Long] = 
        HashSet[Long]() ++ usrMods.filter(_.isUp.is).map(_.annID.is)

    private def getDownMods(usrMods: Set[Mod]): Set[Long] = 
        HashSet[Long]() ++ usrMods.filter(!_.isUp.is).map(_.annID.is)

    private def addAutoUpMod(ann: Annotation) = User.currentUser match {
        case Full(user) => 
            Mod.create.userID(user.id.is)
                      .passageID(ann.passageID.is)
                      .sentenceID(ann.sentenceID.is)
                      .annID(ann.id.is)
                      .isUp(true)
                      .save
        case _ => ()
    }

    private def getDisplayedSentNumOffset(psgID: Long): Int =
        Passage.find(By(Passage.id, psgID)) match {
            case Full(psg) => psg.firstDisplayedSentNum - 1
            case _ => {
                Log.error("Couldn't find psg of id " + psgID)
                0
            }
        }

    private def getChangeViewDiv(psgID: Long): Node = 
        <div id="changeview">Order by 
        {SHtml.ajaxSelect(constructViewChangeList(),
                          Empty,
                          (procViewChange (psgID, _)))
        }
        </div>

    private def constructViewChangeList(): List[(String, String)] = AnnOrdering.is match {
        case Conversation => List(("Conversation", "conversation"), 
                                  ("TopSent", "vote count"))
        case TopSent => List(("TopSent", "vote count"), 
                             ("Conversation", "conversation"))
        case TopPsg => List(("TopPsg", "Top for whole passage"),
                            ("Conversation", "conversation"), 
                            ("TopSent", "top for each sentence"))
       
    }

    //this is stupid, I know. Lift 1.0 doesn't have an ajaxSelectObj. Can be made better when this happens.
    private def procViewChange(psgID: Long, viewStr: String): JsCmd = {
        viewStr match {
            case "Conversation" => AnnOrdering(Conversation)
            case "TopSent" => AnnOrdering(TopSent)
            case "TopPsg" => AnnOrdering(TopPsg)
            case _ => Log.error("got unknown Order type in procViewChange")
        }
        RedirectTo("/passage/"+psgID.toString)
    }

    /**
    @return List of InitAnnList objects
    */
    private def getInitAnnListFromDB(psgID: Long): List[InitAnnList] =
        getSentIDListForPsg(psgID) map {x => getInitAnnList(psgID, x)}

    private def getSentIDListForPsg(psgID: Long): List[Int] =
        try {
            List.range(0, Passage.find(By(Passage.id, psgID)).open_!.numSentences.is +1)
        } catch {
            case e => {
                Log.error("in getSentIDListForPsg, cannot find passage with id " + psgID)
                Nil
            } 
        }

    private def getInitAnnList(psgID: Long, sentID: Int): InitAnnList = {
        val annsPlusOne = AnnOrdering.is match {
            case TopSent =>
                Annotation.findAll(By(Annotation.passageID, psgID), 
                                   By(Annotation.sentenceID, sentID),
                                   OrderBy(Annotation.score, Descending),
                                   StartAt(0),
                                   MaxRows(GlobalConstants.INIT_ANNLIST_LEN + 1))
            case Conversation => 
                Annotation.findAll(By(Annotation.passageID, psgID), 
                                   By(Annotation.sentenceID, sentID),
                                   OrderBySql("annotation.parentAnn asc, annotation.submissionDateTimeMillis asc",
                                              IHaveValidatedThisSQL("karl", "20090607")),
                                   StartAt(0),
                                   MaxRows(GlobalConstants.INIT_ANNLIST_LEN + 1))
            case _ => {
                Log.error("got unknown Order type")
                Nil
            }
        }
        InitAnnList(sentID,
                    annsPlusOne.length > GlobalConstants.INIT_ANNLIST_LEN,
                    if(annsPlusOne.length > GlobalConstants.INIT_ANNLIST_LEN) annsPlusOne.reverse.tail.reverse
                    else annsPlusOne)
    }

    private def getEntireAnnList(psgID: Long, sentID: Int): List[Annotation] = AnnOrdering.is match {
        case TopSent =>
            Annotation.findAll(By(Annotation.passageID, psgID), 
                               By(Annotation.sentenceID, sentID),
                               OrderBy(Annotation.score, Descending))
        case Conversation => 
            Annotation.findAll(By(Annotation.passageID, psgID), 
                               By(Annotation.sentenceID, sentID),
                               OrderBySql("annotation.parentAnn asc, annotation.submissionDateTimeMillis asc",
                                          IHaveValidatedThisSQL("karl", "20090607")))
        case _ => {
            Log.error("got unknown Order type")
            Nil
        }
    }

    /**
        *returns username of author or, if author id not found, returns "" 
    */
    def getAuthorName(ann: Annotation): String =
    {
        val users = User.findAll(By(User.id, ann.authorID.is))
        if(users.length == 1) users(0).name.is
        else ""
    }

    private def constructAnnotationListing(psgID: Long, 
                                           initAnnLists: List[InitAnnList], 
                                           upMods: Set[Long], 
                                           downMods: Set[Long],
                                           displayedSentNumOffset: Int): Node = 
        <div id={commentListDivID}>
            { createAnnotationAndSentIDList(psgID, initAnnLists, upMods, downMods, displayedSentNumOffset) }
        </div>

    private def createAnnotationAndSentIDList(psgID: Long, 
                                              initAnnLists: List[InitAnnList], 
                                              upMods: Set[Long],
                                              downMods: Set[Long],
                                              displayedSentNumOffset: Int): List[Node] =
        initAnnLists.map(x => convertInitAnnListToSentAnnsDiv(psgID,  x, upMods, downMods, displayedSentNumOffset))

    private def convertInitAnnListToSentAnnsDiv(psgID: Long,
                                                initAnns: InitAnnList,
                                                upMods: Set[Long],
                                                downMods: Set[Long],
                                                displayedSentNumOffset: Int): Node =
        initAnns match {
            case InitAnnList(sentID, moreExist, anns) =>
                <div class={if(anns.isEmpty) "sentanns emptysentanns"
                            else "sentanns nonemptysentanns"}
                     id={constructAnnSectionDivID(sentID)}>
                    {createSentTitleDiv(psgID, sentID, anns.isEmpty, displayedSentNumOffset)}
                    {createInitAnnNodesForSent(psgID, sentID, anns, upMods, downMods)}
                    {createDrilldownDiv(psgID, sentID, moreExist, anns)}
                    {createInactiveCancelDDDiv(psgID, sentID)}
                </div> 
        }
    

    /*
     *This is public so the user's display can show comments like they get shown here.
    */
    def createIndAnnotationDisplay(ann: Annotation, 
                                           upMods: Set[Long], 
                                           downMods: Set[Long],
                                           shouldIndentAddedChild: Boolean): Node = 
        <div class="ann">
            {getAnnModDiv(ann, 
                          if(upMods contains ann.id.is) Upvoted 
                          else if(downMods contains ann.id.is) Downvoted
                          else Unvoted)}
            <div class="anntextandinfo">
                {getAnnTextDiv(ann)}
                {getInfoDiv(ann)}
                {getAnnotationFooter(ann)}
                {getReplyGhostDiv(ann, shouldIndentAddedChild)}
            </div>
            <div class="clearall"/>
        </div>

    private def createInitAnnNodesForSent(psgID: Long,
                                          sentID: Int,
                                          anns: List[Annotation], 
                                          upMods: Set[Long],
                                          downMods: Set[Long]): Node = 
        <div class="sentannswrap" id={getSentAnnsWrapDivID(sentID)}>
        {constructAjaxBaseCommentForm(psgID, sentID)}
        {constructSentannswrapInterior(sentID, anns, upMods, downMods)}
        </div>

    private def constructSentannswrapInterior(sentID: Int,
                                              anns: List[Annotation], 
                                              upMods: Set[Long],
                                              downMods: Set[Long]): List[Node] = AnnOrdering.is match {
        case Conversation =>
            partitionAnnsByParentChildLevel(anns).map(x => createParentOrChildNode(x, upMods, downMods))
        case TopSent => anns.map(x => createTopOrderedIndAnnNode(x, upMods, downMods))
        case _ => {
            Log.error("calling createInitAnnNodesForSent with AnnOrdering set to something other than Conversation or TopSent")
            anns.map(x => createTopOrderedIndAnnNode(x, upMods, downMods))
        }
    }

    private def constructDrilledDownSentAnns(psgID: Long,
                                             sentID: Int,
                                             upMods: Set[Long],
                                             downMods: Set[Long]): List[Node] = {
        val anns: List[Annotation] =  getEntireAnnList(psgID, sentID)
        AnnOrdering.is match {
            case Conversation =>
                partitionAnnsByParentChildLevel(anns).map(x => createParentOrChildNode(x, upMods, downMods))
            case TopSent => anns.map(x => createTopOrderedIndAnnNode(x, upMods, downMods))
            case _ => {
                Log.error("calling createInitAnnNodesForSent with AnnOrdering set to something other than Conversation or TopSent")
                anns.map(x => createTopOrderedIndAnnNode(x, upMods, downMods))
            }
        }
    }

    private def constructUnDrilledDownSentAnns(psgID: Long,
                                               sentID: Int,
                                               upMods: Set[Long],
                                               downMods: Set[Long]): List[Node] =
        getInitAnnList(psgID, sentID) match {
            case InitAnnList(_, moreAnnsExist, anns) =>
                constructSentannswrapInterior(sentID, anns, upMods, downMods)
        }


    private def partitionAnnsByParentChildLevel(anns: List[Annotation]): List[List[Annotation]] = {
        var reversedResult: List[List[Annotation]] = Nil
        var curReversed: List[Annotation] = Nil
        anns.foreach(x => 
            if(x.inReplyTo.is == x.id.is) {
                if(curReversed.isEmpty) reversedResult = List(x) :: reversedResult
                else {
                    reversedResult = curReversed.reverse :: reversedResult
                    reversedResult = List(x) :: reversedResult
                    curReversed = Nil
                }
            }
            else curReversed = x :: curReversed
        )
        if(!curReversed.isEmpty) reversedResult = curReversed.reverse :: reversedResult
        reversedResult.reverse
    }

    private def createParentOrChildNode(anns: List[Annotation], 
                                        upMods: Set[Long],
                                        downMods: Set[Long]): Node = 
        <div class={if(anns.isEmpty) "emptyanns"
                    else if(anns(0).id.is == anns(0).inReplyTo.is) "parentanns"
                    else "childanns"}>
            {anns.map(x => createIndAnnotationDisplay(x,
                                                      upMods,
                                                      downMods,
                                                      !(anns.isEmpty) &&
                                                        anns.head.id.is == anns.head.inReplyTo.is))}
        </div>


    /**
    *@return Node elem for individual annotation when user is viewing based on Top Order.
    */
    private def createTopOrderedIndAnnNode(ann: Annotation, upMods: Set[Long], downMods: Set[Long]): Node =
        <div class="parentanns">{createIndAnnotationDisplay(ann, upMods, downMods, false)}</div>


    private def createSentTitleDiv(psgID: Long,
                                   sentID: Int,
                                   isEmpty: Boolean,
                                   displayedSentNumOffset: Int): Node = 
        <div class="annsenttit">
            <div class="annnum" id={constructAnnSectionDivID(sentID)}>
                {SHtml.a(
                    if(sentID == 0) Text("Comments on Entire Passage")
                     else Text("Line "+(sentID+displayedSentNumOffset)),
                    Run("scrollLHSToSent(" + sentID + ");"),
                    ("name", getSentAnchorName(sentID))
                    )
                }
            </div>
        </div>

    private def createDrilldownDiv(psgID: Long,
                                   sentID: Int,
                                   moreExist: Boolean,
                                   anns: List[Annotation]): Node = {
        if(moreExist)
                {SHtml.a(
                    (() =>  {
                        val usrMods: Set[Mod] = getUsrModsForSent(psgID, sentID)
                        val upMods: Set[Long] = getUpMods(usrMods)
                        val downMods: Set[Long] = getDownMods(usrMods)
                        Run("drillDown(" +
                            "'" + constructAnnSectionDivID(sentID) + "', "  +
                            "'" +getSentAnnsWrapDivID(sentID) + "'" +
                            ");") &
                        SetHtml(getCancelDDDivID(sentID),
                                createActiveCancelDDDiv(psgID, sentID, upMods, downMods)) &
                        SetHtml(getSentAnnsWrapDivID(sentID),
                                constructDrilledDownSentAnns(psgID,sentID, upMods, downMods))
                    }),
                    <div class="dd">See all...</div>
                )}
        else
            <div class="emptydd">
            </div>
    }


    private def createInactiveCancelDDDiv(psgID: Long, sentID: Int) =
        <div class="canceldd inactcanceldd" id={getCancelDDDivID(sentID)}/>

    private def createActiveCancelDDDiv(psgID: Long,
                                        sentID: Int,
                                        upMods: Set[Long],
                                        downMods: Set[Long]) =
        SHtml.a(
            (() =>
                Run("unDrillDown()") &
                SetHtml(getCancelDDDivID(sentID), createInactiveCancelDDDiv(psgID, sentID)) &
                SetHtml(getSentAnnsWrapDivID(sentID),
                        constructUnDrilledDownSentAnns(psgID, sentID, upMods, downMods))
            ),
            <div class="canceldd actcanceldd" id={getCancelDDDivID(sentID)}>(Done)</div>
            )

    private def getAnnTextDiv(ann: Annotation): Node =
        <div class="anntext" id={getAnnTextID(ann)}>
            {constructDisplayableAnnText(ann.commentText.is)}
        </div>

    private def constructDisplayableAnnText(rawText: String): List[Node] =
        InterTextUtil.resolveIntertextualReferences(
            ((rawText.split("<br />").map(Text(_))
                zip
              rawText.split("<br />").map(x => <br />)
             ).toList.flatten( x => List(x._1, x._2))))

    private def getAnnModDiv(ann: Annotation, vote: Vote): Node =
        <div class={getAnnModDivClass(vote)}>
            {getUpmodDiv(ann, vote)}
            {getAnnPopDiv(ann)}
            {getDownmodDiv(ann, vote)}
        </div>

    private def getAnnModDivClass(vote: Vote): String = vote match {
        case Upvoted => "annmod upvoted"
        case Downvoted => "annmod downvoted"
        case Unvoted => "annmod unvoted"
    }

    private def getUpmodDiv(ann: Annotation, vote: Vote): Node = 
        <div id={getUpmodDivID(ann)}>
            {vote match {
             case Upvoted => <div class="arr upvoted"></div>
             case _ =>
                SHtml.a(User.currentUser match {
                            case Full(_) => upmod(ann, vote)
                            case _ => () => Alert("please log in to vote")}, 
                        <div class="arr up"></div>, 
                        )
             }}
        </div>

    private def getDownmodDiv(ann: Annotation, vote: Vote): Node =
        <div id={getDownmodDivID(ann)}>
            {vote match {
             case Downvoted => <div class="arr downvoted"></div>
             case _ =>
                SHtml.a(User.currentUser match {
                            case Full(_) => downmod(ann, vote)
                            case _ => () => Alert("please log in to vote")}, 
                        <div class="arr down"></div>, 
                        )
             }}
        </div>


    private def getAnnPopDiv(ann: Annotation): Node = 
        Elem(null, "div", new UnprefixedAttribute("id", getPopularityID(ann), Null), TopScope,
            Text(ann.score.is.toString)
        )

    private def getUpmodDivID(ann: Annotation): String = "upmod"+ann.id.is
    private def getDownmodDivID(ann: Annotation): String = "downmod"+ann.id.is

    private def getPopularityID(ann: Annotation): String = "annpop" + ann.id.is

    private def getInfoDiv(ann: Annotation): Node = 
        Elem(null, "div", new UnprefixedAttribute("class", "anninfo", Null), TopScope,
            Text(getTimeAgoStr(ann) + " by "),  getAnnotationAuthorLink(ann), getInReplyToText(ann)
        )

    def getTimeAgoStr(ann: Annotation): String = {
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

    private def getAnnotationAuthorLink(ann: Annotation): Node =
            if(getAuthorName(ann) != "")
                Elem(null, "a", new UnprefixedAttribute("href", "/user/"+getAuthorName(ann), Null), TopScope, Text(getAuthorName(ann)))
            else Text("Anonymous")

    private def getInReplyToText(ann: Annotation): Node = 
        if(ann.inReplyTo.is == ann.id.is) <div class="annrepinf emptyrepinf"></div>
        else <div class="annrepinf">in reply to 
               {try {
                    getAnnotationAuthorLink(Annotation.find(By(Annotation.inReplyTo, ann.inReplyTo.is)).open_!)
                } catch {
                    case e => Log.error("in getInReplyToText, couldn't find ann via inReplyTo for " + ann.inReplyTo.is)
                } }
             </div>

    private def constructAjaxBaseCommentForm(psgID: Long, sentID: Int): Node = User.currentUser match {
        case Full(_) => 
            <div class="anncom">
                {getBaseCommentSpan(psgID, sentID)}
                {getEmptyBaseCommentFormNode(sentID)}
                {getBaseGhostDiv(getBaseGhostDivID(sentID))}
            </div>
        case _ => <div class="anncom notloggedinanncom"/>
    }


    private def getAnnotationFooter(ann: Annotation): Node = User.currentUser match {
        case Full(user) => getLoggedInAnnotationFooter(ann, user)
        case _ => getLoggedOutAnnotationFooter(ann)
    }

    private def getReplyGhostDiv(ann: Annotation, shouldIndentAddedChild: Boolean): Node =
        <div class={if(shouldIndentAddedChild) "childanns" else "parentanns"}
             id={getReplyGhostDivID(ann.id.is.toString)} />
    private def getBaseGhostDiv(identifier: String): Node =
        <div class="parentanns" id={identifier} />

    private def getLoggedInAnnotationFooter(ann: Annotation, user: User): Node =
        <div class="annfooter">
            {getReplyLinkSpan(ann)}
            {getEditSpan(ann, user)}
            {getEmptyReplyFormDiv(ann)}
        </div>

    private def getLoggedOutAnnotationFooter(ann: Annotation): Node =
        <div class="annfooter">
            <span class="anncom notloggedinanncom" />
        </div>

    private def getEditSpan(ann: Annotation, user: User): Node =
        if(user.id.is == ann.authorID.is)
            <span class="annedit">
                {(SHtml.a(
                    () =>
                        SetHtml(getAnnTextID(ann), constructEditAnnForm(ann)) &
                        Run("document.getElementById('" + getAnnEditTextAreaID(ann) + "').focus()"),
                    Text("edit")
                ))}
            </span>
        else
            <span class="annedit"/>

    /**
    * Returns ajax link with span "commentsent"+id 
    *@param id the unique id of the annotation (id in DB row)
    */
    private def getBaseCommentSpan(psgID: Long, sentID: Int): Node =
        <span id={constructCommentSentDivID(sentID)} class="anncomtxt">
            {getBaseCommentLink(psgID, sentID)}
        </span>

    private def getBaseCommentLink(psgID: Long, sentID: Int): Node =
        SHtml.a(() => activateBaseCommentLink(psgID, sentID), Text("Make a comment"))

    def activateBaseCommentLink(psgID: Long, sentID: Int): JsCmd = User.currentUser match {
        case Full(_) =>
            SetHtml(constructCommentSentDivID(sentID), getCancelBaseLink(psgID, sentID)) &
            SetHtml(constructCommentSentFormDivID(sentID), getNonemptyBaseCommentFormNode(psgID, sentID)) &
            Run("document.getElementById('" + constructCommentSentTextAreaID(sentID) + "').focus();")
        case _ => Noop
    }

    private def getCancelBaseLink(psgID: Long, sentID: Int): Node =
        Elem(null, "span", new UnprefixedAttribute("id", constructCommentSentDivID(sentID), Null), TopScope,
            SHtml.a(() => deactivateBaseCommentLink(psgID, sentID), Text("cancel"))
        )

    def deactivateBaseCommentLink(psgID: Long, sentID: Int): JsCmd = User.currentUser match {
        case Full (_) =>
            SetHtml(constructCommentSentDivID(sentID), getBaseCommentLink(psgID, sentID))  &
            SetHtml(constructCommentSentFormDivID(sentID), getEmptyBaseCommentFormNode(sentID))
        case _ => Noop
    }

    private def getEmptyBaseCommentFormNode(sentID: Int): Node =
        Elem(null, "div", new UnprefixedAttribute("id", constructCommentSentFormDivID(sentID), Null), TopScope)

    private def getNonemptyBaseCommentFormNode(psgID: Long, sentID: Int): Node = {
        var ann: Annotation = null
        SHtml.ajaxForm(
            <div class="comform">
                {SHtml.textarea("",
                               (x => ann = addNewBaseAnnotation(cleanUpAnnText(x), psgID, sentID)),
                               ("class", "anntextarea"),
                               ("id", constructCommentSentTextAreaID(sentID)))}
                {SHtml.submit("add comment", () => ())}
                {(SHtml.ajaxButton(
                    "cancel",
                    () => deactivateBaseCommentLink(psgID, sentID)
                ))}
            </div>,
            deactivateBaseCommentLink(psgID, sentID) &
            SHtml.ajaxInvoke(() => addNewAnnotationToDisplay(ann,
                                    getBaseGhostDivID(sentID)))._2.cmd
        )
    }



    /**
    * Returns ajax link with span "replyto"+id 
    *@param id the unique id of the annotation (id in DB row)
    */
    private def getReplyLinkSpan(ann: Annotation): Node =
        <span id={constructReplyToDivID(ann.id.is)}>
            {SHtml.a(() => activateReplyLink(ann), Text("reply"))}
        </span>

    def activateReplyLink(ann: Annotation): JsCmd = User.currentUser match {
        case Full(_) =>
            SetHtml(constructReplyToDivID(ann.id.is), getCancelReplyLink(ann))  &
            SetHtml(constructReplyFormDivID(ann.id.is), getNonemptyReplyFormNode(ann)) &
            Run("document.getElementById('" + constructReplyTextAreaID(ann.id.is) + "').focus()")
        case _ => Noop
    }

    private def getCancelReplyLink(ann: Annotation): Node = 
        Elem(null, "span", new UnprefixedAttribute("id", constructReplyToDivID(ann.id.is), Null), TopScope,
            SHtml.a( () => deactivateReplyLink(ann), Text("cancel"))
        )

    def deactivateReplyLink(ann: Annotation): JsCmd =
        SetHtml(constructReplyToDivID(ann.id.is), getReplyLinkSpan(ann)) &
        SetHtml(constructReplyFormDivID(ann.id.is), getEmptyReplyFormDiv(ann))

    private def getEmptyReplyFormDiv(parentann: Annotation) =
        Elem(null, "div", new UnprefixedAttribute("id", "replyform"+parentann.id.is.toString, Null), TopScope)

    private def getNonemptyReplyFormNode(parentann: Annotation) = {
        var ann: Annotation = null
        SHtml.ajaxForm(
            <div class="comform">
                {SHtml.textarea("",
                               (x => ann = addReply(cleanUpAnnText(x),parentann)),
                               ("class", "anntextarea"),
                               ("id", constructReplyTextAreaID(parentann.id.is)))}
                {SHtml.submit("add reply", () => ())}
                {(SHtml.ajaxButton(
                    "cancel",
                    () => deactivateReplyLink(parentann)
                ))}
            </div>,
            deactivateReplyLink(parentann) &
            SHtml.ajaxInvoke(() => addNewAnnotationToDisplay(ann,
                                    getReplyGhostDivID(parentann.id.is.toString)))._2.cmd
        )
    }

    private def constructEditAnnForm(ann: Annotation): Node = {
        SHtml.ajaxForm(
            <div class="anneditform">
                {(SHtml.textarea(
                    ann.commentText.is.replaceAll("<br />", "\n"),
                    (x => {
                        updateAnn(cleanUpAnnText(x), ann)
                    }),
                    ("class", "annedittextarea"),
                    ("id", getAnnEditTextAreaID(ann))
                ))}
                {(SHtml.submit(
                    "save",
                    () => ()
                ))}
                {(SHtml.ajaxButton(
                    "cancel",
                    () => SetHtml(getAnnTextID(ann), constructDisplayableAnnText(ann.commentText.is))
                ))}
            </div>,
            (SHtml.ajaxInvoke(() =>
                SetHtml(getAnnTextID(ann), constructDisplayableAnnText(ann.commentText.is))))._2.cmd
        )
    }



    private def addNewAnnotationToDisplay(ann: Annotation, idOfContainerDiv: String): JsCmd =
        Run("prependToHtml(" + idOfContainerDiv.encJs + ", "+
            fixHtml("foo", createIndAnnotationDisplay(ann,
                                       HashSet[Long](ann.id.is),
                                       HashSet[Long](),
                                       false)) +
            ")")
    
    private def addReply(anntext: String, parentann: Annotation): Annotation = {
        val ann = Annotation.create
        val curUser = getCurUser
        if(curUser != -1)
            ( ann.inReplyTo(parentann.id.is)
                .commentText(anntext)
                .score(1)
                .passageID(parentann.passageID.is)
                .sentenceID(parentann.sentenceID.is)
                .submissionDateTimeMillis(new DateTime().getMillis)
                .authorID(curUser)
                .replyOrder(parentann.numReplies.is + 1)
                .parentAnn(parentann.parentAnn.is)
                .save
            )
        else
            ( ann.inReplyTo(parentann.id.is)
                .commentText(anntext)
                .score(1)
                .passageID(parentann.passageID.is)
                .sentenceID(parentann.sentenceID.is)
                .submissionDateTimeMillis(new DateTime().getMillis)
                .replyOrder(parentann.numReplies.is + 1)
                .parentAnn(parentann.parentAnn.is)
                .save
            )
        parentann.numReplies(parentann.numReplies.is +1).save
        addAutoUpMod(ann)
        ann
    }

    private def updateAnn(newText: String, ann: Annotation) = 
        ann.commentText(newText).lastEditDateTimeMillis(new DateTime().getMillis).save

    private def upmod(ann: Annotation, curVote: Vote): () => JsCmd  =  curVote match {
        case Downvoted => upmodAsUndoOfDownvote(ann)
        case Unvoted => upmodAsFirstVote(ann)
        case Upvoted => {Log.warn("function upmod called with curVote == Upvoted"); () => Noop}
    }

    private def upmodAsUndoOfDownvote(ann: Annotation): () => JsCmd = 
        () => {
            val newScore = ann.score.is + 2
            ann.score(ann.score.is + 2).save
            User.currentUser match {
                case Full(user) => 
                    try {
                        (Mod.findAll(By(Mod.passageID, ann.passageID.is),
                                                By(Mod.userID, user.id.is),
                                                By(Mod.annID, ann.id.is))(0)
                                             .isUp(true)
                                             .save)
                     } catch {
                        case e => Log.error("at upmodAsUndoOfDownvote, bad fetch from DB")
                     }
                case _ => Log.warn("upmodAsFirstVote called while user not logged in")
            }
            (SetHtml(getUpmodDivID(ann), <div class="arr upvoted"></div>)
              &
            SetHtml(getPopularityID(ann), Text(newScore.toString))
              &
            SetHtml(getDownmodDivID(ann), getDownmodDiv(ann, Upvoted)))
        }

    private def upmodAsFirstVote(ann: Annotation): () => JsCmd =
        () => {
            val newScore = ann.score.is + 1
            ann.score(ann.score.is + 1).save
            User.currentUser match {
                case Full(user) => (Mod.create.userID(user.id.is)
                                             .passageID(ann.passageID.is)
                                             .sentenceID(ann.sentenceID.is)
                                             .annID(ann.id.is)
                                             .isUp(true)
                                             .save)
                case _ => Log.warn("upmodAsFirstVote called while user not logged in")
            }
            (SetHtml(getUpmodDivID(ann), <div class="arr upvoted"></div>)
              &
            SetHtml(getPopularityID(ann), Text(newScore.toString))
              &
            SetHtml(getDownmodDivID(ann), getDownmodDiv(ann, Upvoted)))
        }

    private def downmod(ann: Annotation, curVote: Vote): () => JsCmd  =  curVote match {
        case Upvoted => downmodAsUndoOfUpvote(ann)
        case Unvoted => downmodAsFirstVote(ann)
        case Downvoted => {Log.warn("function downmod called with curVote == Downvoted"); () => Noop}
    }

    private def downmodAsUndoOfUpvote(ann: Annotation): () => JsCmd = 
        () => {
            val newScore = ann.score.is - 2
            ann.score(ann.score.is - 2).save
            User.currentUser match {
                case Full(user) => 
                    try {
                        (Mod.findAll(By(Mod.passageID, ann.passageID.is),
                                                By(Mod.userID, user.id.is),
                                                By(Mod.annID, ann.id.is))(0)
                                             .isUp(false)
                                             .save)
                     } catch {
                        case e => Log.error("at downmodAsUndoOfUpvote, bad fetch from DB")
                     }
                case _ => Log.warn("upmodAsFirstVote called while user not logged in")
            }
            (SetHtml(getDownmodDivID(ann), <div class="arr downvoted"></div>)
              &
            SetHtml(getPopularityID(ann), Text(newScore.toString))
              &
            SetHtml(getUpmodDivID(ann), getUpmodDiv(ann, Downvoted)))
        }

    private def downmodAsFirstVote(ann: Annotation): () => JsCmd = 
        () => {
            val newScore = ann.score.is - 1
            ann.score(ann.score.is - 1).save
            User.currentUser match {
                case Full(user) => (Mod.create.userID(user.id.is)
                                             .passageID(ann.passageID.is)
                                             .sentenceID(ann.sentenceID.is)
                                             .annID(ann.id.is)
                                             .isUp(false)
                                             .save)
                case _ => Log.warn("upmodAsFirstVote called while user not logged in")
            }
            (SetHtml(getDownmodDivID(ann), <div class="arr downvoted"></div>) &
             SetHtml(getPopularityID(ann), Text(newScore.toString)) &
             SetHtml(getUpmodDivID(ann), getUpmodDiv(ann, Downvoted)))
        }


    private def cleanUpAnnText(text: String) =
        text.replaceAll("\n\\s*\n\\s*\n", "\n\n").trim.replaceAll("\n", "<br />")



    val commentListDivID = "commentlist"
    val emptyAnnWeight = 0.2

    def getSentAnchorName(sentID: Int): String = sentID.toString
    def getSentTextAnchorName(sentID: Int): String = sentID.toString + "text"
    private def constructReplyToDivID(annID: Long) = "replyto" + annID.toString
    private def constructReplyFormDivID(annID: Long) = "replyform" + annID.toString
    private def constructReplyTextAreaID(annID: Long) = "replytextarea" + annID
    private def constructCommentSentDivID(sentID: Int): String = "commentsent" + sentID
    private def constructCommentSentFormDivID(sentID: Int): String = "commentsentform" + sentID
    private def constructCommentSentTextAreaID(sentID: Long) = "commenttextarea" + sentID

    //This one's important because this is a naming convention we reproduce in client-side
    //code so that we can have scrolling sync.
    private def constructAnnSectionDivID(sentID: Int) = "anns" + sentID

    private def getCancelDDDivID(sentID: Int) = "cancel" + sentID

    private def getSentAnnsWrapDivID(sentID: Int) = "sawrp" + sentID

    private def getAnnTextID(ann: Annotation) = "at" + ann.id.is
    private def getAnnEditTextAreaID(ann: Annotation) = "ate" + ann.id.is

    private def getReplyGhostDivID(identifier: String) = "rg" + identifier
    private def getBaseGhostDivID(sentID: Int) = "bg" + sentID

    abstract class Order
    case object Conversation extends Order
    case object TopPsg extends Order
    case object TopSent extends Order

    abstract class Vote
    case object Upvoted extends Vote
    case object Downvoted extends Vote
    case object Unvoted extends Vote



    /**
    * Thing storing info on initial annotations to display (and annotations themselves)
    */
    case class InitAnnList(sentID: Int, moreAnnsExist: Boolean, initAnns: List[Annotation])

}
