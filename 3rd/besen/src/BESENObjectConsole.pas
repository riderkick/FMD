(*******************************************************************************
                                 L I C E N S E
********************************************************************************

BESEN - A ECMAScript Fifth Edition Object Pascal Implementation
Copyright (C) 2009-2015, Benjamin 'BeRo' Rosseaux

The source code of the BESEN ecmascript engine library and helper tools are 
distributed under the Library GNU Lesser General Public License Version 2.1 
(see the file copying.txt) with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a module
which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but you 
are not obligated to do so. If you do not wish to do so, delete this exception
statement from your version.

If you didn't receive a copy of the license, see <http://www.gnu.org/licenses/>
or contact:
      Free Software Foundation
      675 Mass Ave
      Cambridge, MA  02139
      USA

*******************************************************************************)
unit BESENObjectConsole;
{$i BESEN.inc}

interface

const BESENObjectConsoleSource:{$ifdef BESENSingleStringType}TBESENSTRING{$else}widestring{$endif}=
'/**'+#13#10+
' * Console object for BESEN'+#13#10+
' * @author Dmitry A. Soshnikov <dmitry.soshnikov@gmail.com>'+#13#10+
' */'+#13#10+
'(function initConsole(global) {'+#13#10+
''+#13#10+
'  // helpers'+#13#10+
''+#13#10+
'  var getClass = Object.prototype.toString;'+#13#10+
'  var timeMap = {};'+#13#10+
''+#13#10+
'  function repeatSring(string, times) {'+#13#10+
'    return Array(times + 1).join(string);'+#13#10+
'  }'+#13#10+
''+#13#10+
'  function dir(object, deep, level) {'+#13#10+
'    level || (level = 1);'+#13#10+
'    typeof deep == "undefined" && (deep = true);'+#13#10+
'    var openBracket, closeBracket;'+#13#10+
'    if (Array.isArray(object)) {'+#13#10+
'      openBracket = "["; closeBracket = "]"'+#13#10+
'    } else {'+#13#10+
'      openBracket = "{"; closeBracket = "}"'+#13#10+
'    }'+#13#10+
'    var props = [];'+#13#10+
'    var indent = repeatSring(console.dir.indention, level);'+#13#10+
'    if (console.dir.showInternals) {'+#13#10+
'      props.push(indent + "[[Class]]: \"" + getClass.call(object).slice(8, -1) + "\"");'+#13#10+
'    }'+#13#10+
'    var data, current, currentClass;'+#13#10+
'    var goDeeper = (typeof deep == "number" ? deep > level : deep);'+#13#10+
'    Object.getOwnPropertyNames(object).forEach(function (property) {'+#13#10+
'      current = object[property];'+#13#10+
'      currentClass = getClass.call(current);'+#13#10+
'      if (goDeeper && (currentClass == "[object Object]" || Array.isArray(current))) {'+#13#10+
'        data = dir(current, deep, level + 1);'+#13#10+
'      } else {'+#13#10+
'        data = ('+#13#10+
'          typeof current == "function" ? "function" : ('+#13#10+
'            Array.isArray(current) ? "[" + current + "]" :'+#13#10+
'            (currentClass == "[object String]" ? "\""  + current + "\"" : current)'+#13#10+
'          )'+#13#10+
'        );'+#13#10+
'      }'+#13#10+
'      props.push(indent + property + ": " + data);'+#13#10+
'    });'+#13#10+
'    return "".concat('+#13#10+
'      openBracket, "\n", props.join(",\n"), "\n",'+#13#10+
'      (level > 1 ? repeatSring(console.dir.indention, level - 1) : ""),'+#13#10+
'      closeBracket'+#13#10+
'    );'+#13#10+
'  }'+#13#10+
''+#13#10+
'  /**'+#13#10+
'   * console object;'+#13#10+
'   * implements: log, dir, time, timeEnd'+#13#10+
'   */'+#13#10+
'  global.console = {'+#13#10+
''+#13#10+
'    /**'+#13#10+
'     * simple log using toString'+#13#10+
'     */'+#13#10+
'    log: function(){'+#13#10+
'      var s = "", a = arguments, j = +a.length;'+#13#10+
'      for(var i=0;i<j;i++) s += a[i] + " ";'+#13#10+
'      println(s);'+#13#10+
'    },'+#13#10+
''+#13#10+
'    /**'+#13#10+
'     * dir an object'+#13#10+
'     * @param {Object} object'+#13#10+
'     * @param {Variant} deep - level of depth, default is {Boolean} true'+#13#10+
'     * can be set also to {Number} value specifying needed level of depth'+#13#10+
'     * Examples:'+#13#10+
'     * - console.dir(obj) // console.dir(obj, true)'+#13#10+
'     * - console.dir(obj, false); // only first level is shown'+#13#10+
'     * - console.dir(obj, 3); // properties of three levels are shown'+#13#10+
'     */'+#13#10+
'    dir: function (object, deep) {'+#13#10+
'      // if called for a primitive'+#13#10+
'      if (Object(object) !== object) {'+#13#10+
'        return console.log(object);'+#13#10+
'      }'+#13#10+
'      // else for an object'+#13#10+
'      return println(dir(object, deep));'+#13#10+
'    },'+#13#10+
''+#13#10+
'    // time functions borrowed from Firebug'+#13#10+
''+#13#10+
'    /**'+#13#10+
'     * time start'+#13#10+
'     */'+#13#10+
'    time: function(name) {'+#13#10+
'      timeMap[name] = Date.now();'+#13#10+
'    },'+#13#10+
''+#13#10+
'    /**'+#13#10+
'     * time end'+#13#10+
'     */'+#13#10+
'    timeEnd: function(name) {'+#13#10+
'      if (name in timeMap) {'+#13#10+
'        var delta = Date.now() - timeMap[name];'+#13#10+
'        println(name + ": ", delta + "ms");'+#13#10+
'        delete timeMap[name];'+#13#10+
'      }'+#13#10+
'    }'+#13#10+
'  };'+#13#10+
''+#13#10+
'  // indention for dir, default is 4 spaces'+#13#10+
'  console.dir.indention = "    ";'+#13#10+
''+#13#10+
'  // whether to show internal properties such as [[Class]]'+#13#10+
'  console.dir.showInternals = true;'+#13#10+
''+#13#10+
'})(this);'+#13#10;

implementation

end.
