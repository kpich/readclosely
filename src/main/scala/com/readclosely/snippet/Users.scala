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

import scala.xml._
import com.readclosely.model._
import com.readclosely.util._

class Users {

    def viewUser(xhtml: NodeSeq): NodeSeq = S.param("username") match {
        case Full(username) => {
            try {
                UserUtil.getUserProfileNode(username)
            } catch {
                case e: Exception => {
                    Log.warn("Couldn't find user with username " + username.toString)
                    Text("Error in processing :(")
                    }
            }
        }
        case _ => {
            Log.warn("Error in looking up user (parameter 'username' not present or well-formed)")
            Text("User not found :(")
        }
    }


}

