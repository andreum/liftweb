/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.builtin.snippet

import net.liftweb.http._
import scala.xml._

class CometScript {
  def javaScript: NodeSeq = {
    val uri = S.encodeURL(S.request.map(_.contextPath).openOr("") + S.uri)

    (<script type="text/javascript">
    // {Unparsed(
    """<![CDATA[
      function lift_handlerSuccessFunc(foo) {
        setTimeout("lift_cometEntry();",100);
      }

      function lift_handlerFailureFunc(foo) {
        // if we fail, wait 10 seconds to retry
        setTimeout("lift_cometEntry();",10000);
      }

      function lift_cometEntry() {
        new Ajax.Request('"""+uri+"""', { asynchronous:true, onSuccess:lift_handlerSuccessFunc, onFailure:lift_handlerFailureFunc, method: 'put', requestHeaders:{ Accept:'text/javascript' }});
      }
      // ]]>""")}
    </script>)
  }

  def bodyAttr: MetaData = new UnprefixedAttribute("onload", Text("lift_cometEntry();"), Null)
}
