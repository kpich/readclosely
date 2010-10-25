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


class Footer {

    def render:Node = 
        <div id="footer">
            Use of this site constitutes acceptance of the <a href="/terms">Terms of use</a>.&nbsp;{tagline}
        </div>

    private def tagline: String = lines(new Random().nextInt(lines.length))

    private val lines: Array[String] = 
        (//"No need to read alone." ::
        //"Read real good now, hear?" ::
        //"It's not just you. No one understands TS Eliot." ::
        //"Read with the rest of the world." ::
        //"Crowdsourcing Scholarship." ::
        //"Take back the written word." ::
        //"Excellent." ::
        //"Prove to the world not everyone on the Internet is dumb." ::
        "Yay reading!" ::
        Nil)
        .toArray
}
