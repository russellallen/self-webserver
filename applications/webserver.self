 '$Revision:$'
 '
Copyright 1992-2014 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules webserver.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn revision subpartNames.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- 'applications'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '$Revision:$'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> () From: ( | {
         'Category: applications\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         webserver = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         deadProcess = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'deadProcess' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver deadProcess.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         abort = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isAlive = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'deadProcess' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         isNil = bootstrap stub -> 'globals' -> 'false' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (true)'
        
         debug <- bootstrap stub -> 'globals' -> 'true' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         defaultServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver defaultServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: req = ( |
            | webserver response copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'defaultServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         deregisterServlet = ( |
            | 
            servlet: defaultServlet.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         exampleServlets = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         fileServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets fileServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'~/Sites/\')'
        
         baseDirectory <- '~/Sites/'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         canonicalizeUrl: aPath = ( |
             path.
            | 
            aPath size = 0 ifTrue: [ ^aPath ].
            path: aPath asTokensSeparatedByCharactersIn: '/'.

            "Remove occurences of ./ from path"
            path: path filterBy: [ |:s| s != '.' ].
            path isEmpty ifTrue: [ ^'' ].

            "Remove leading ../ from path"
            [ path isEmpty not && [ path first = '..' ] ] whileTrue: [
              path removeFirst
            ].

            "Process other ../ in path"
            path rep doLinks: [ |:lnk|
              lnk value = '..' ifTrue: [
                lnk prev remove.
                path size: path size pred.
                lnk remove.
                path size: path size pred.
              ].
            ].

            "Get string from path list"
            path isEmpty ifTrue: [ ^'' ].
            path: path reduceWith: [|:a. :b| a,'/',b ] IfSingleton: [ |:a| a ].
            (aPath at: 0) = '/' ifTrue: [ path: '/',path ].
            path).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: u = ( |
             f.
             fn.
             path.
             r.
            | 
            (u url = '') ifTrue: [fn: 'index.html'] False: [fn: u url]. 
            (fn last = '/') ifTrue: [fn: fn, 'index.html'].
            path: baseDirectory, fn.
            path: os_file expand: path.
            path: canonicalizeUrl: path.
            ((os_file expand: baseDirectory) isPrefixOf: path) ifFalse: [
              r: webserver response copy.
              r contents: 'Not Found:', fn.
              r statusCode: '404 Not Found'.
              ^r
            ].
            f: os_file deadCopy openForReading: path IfFail: [ 
              r: webserver response copy.
              r contents: 'Not Found:', fn.
              r statusCode: '404 Not Found'.
              ^r
            ].
            r: webserver response copy.
            r contents: f contents.
            r guessContentTypeFromName: fn.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'fileServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         lobbyBrowserServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         appropriateSigilFor: slot = ( |
            | 
            ( slot mirror at: (slot name, ':') IfAbsent: [^ ' = '] ) 
              value = mirrors assignment ifTrue: [^ ' &larr; '].
            ' = ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         getMirrorAtPath: p = ( |
             o.
            | 
            o: reflect: lobby.
            p do: [|:n| o:(o at: n) contents].
            o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: u = ( |
             o.
             r.
             t.
            | 
            u url = '' 
              ifTrue: [o: reflect: lobby]
              False: [
                o: getMirrorAtPath: 
                  u url asTokensSeparatedByCharactersSatisfying: [|:c | c = '/']].
            r: webserver response copy.
            r contents: htmlFor: o.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         htmlFor: o = ( |
             r.
            | 

            r: '<html><body><h1>', (o evalName), '</h1><p>'.
            r: r, '<i>', (o comment), '</i><p>'.
            r: r, 'CopyDowns: ', (o copyDowns printString), '<p>'.
            r: r, 'CreatorPath: ', (o creatorPath printString), '<p>'.
            r: r, 'ModuleNames: ', (o moduleNames printString), '<p>'.

            r: r, 'Parents:<ul>'.
            o parentsDo: [|:i| r: r, htmlForSlot: i].
            r: r, '</ul>Slots:<ul>'.
            o withoutAssignmentSlotsDo: [|:i| 
                i isParent ifFalse: [r: r, htmlForSlot: i]].

            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         htmlForSlot: s = ( |
            | 
            '<li><a href="/', 
             (transform: s value safeName), '/">', 
             s key, '</a>', (appropriateSigilFor: s),
             s value safeName, '</li>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         transform: s = ( |
            | 
            s replace: ' ' With: '/').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         staticSiteServer = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets staticSiteServer.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         baseDir <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'Category: private\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         cache <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets staticSiteServer p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         filesInDir: d = ( |
            | (os outputOfCommand: ('find ', d, ' -type f') Delay: 100 IfFail: '') asTokensSeparatedByWhiteSpace).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handle: u = ( |
             f.
             fn.
             r.
            | 
            (u url = '') ifTrue: [fn: 'index.html'] False: [fn: u url]. 
            (fn last = '/') ifTrue: [fn: fn, 'index.html'].
            r: webserver response copy.
            r contents: cache at: fn IfAbsent: ['<html><body>Not Found</body></html>'].
            r guessContentTypeFromName: fn.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         loadTreeFrom: dir = ( |
            | baseDir: dir. reloadTree. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'staticSiteServer' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         reloadTree = ( |
            | 
            cache: dictionary copyRemoveAll.
            (filesInDir: baseDir) do: [|:fn. f. s|
              f: os_file openForReading: fn.
              s: f contents.
              f close.
              cache at: (fn copyFrom: baseDir size + 1 UpTo: fn size)
                   Put: s].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         handleRequest: io = ( |
             broken.
             r.
             req.
            | 
            broken: [
              io write: 'HTTP/1.0 501\n\n' IfFail: false.
              io closeIfFail: false. ^ self]. 

            req: request copy readHeader: io IfFail: broken.
            r: safeHandle: req               IfFail: broken.
            r writeHeaderOn: io              IfFail: broken.
            "Ignore body if only want head"
            req method != 'HEAD' ifTrue: [
                 r writeBodyOn: io IfFail: broken].
            io closeIfFail: false.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         initialiseSocketOn: port = ( |
            | 
            serverSocket closeIfFail: [].
            serverSocket:
              os_file
                openTCPListenerOnPort: port
                IfFail: [|:e|
                     (e matchesPattern: '*UNKNOWN 125*')
                  || [e matchesPattern: '*EADDRINUSE*']
                     ifTrue: [error: e, '\n',
                                'Warning: couldn\'t start the rself server process.\n',
                                'The port (', port printString, ') is already in use, ',
                                'probably by another Self server.\n\n',
                                'If you wish to start it, kill the other one off and\n',
                                'evaluate "socketServer start"'.
                              ^self]
                      False: [^error: 'Couldn\'t start self server: ', e]].
            log: 'serverSocket initialised'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         isRunning = ( |
            | 
            serverProcess isAlive).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: logging\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         log: s = ( |
            | 
            debug ifTrue: [s value printString shrinkwrapped printLine]. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'oddball' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: patching\x7fComment: This is an alternative to loading an altered
strings module for 4.5 systems.\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         patchForWhiteSpace = ( |
            | 
            " Tests if Self's defintion of whitespace has been fixed "
            (traits string whiteSpace includes: '\r') ifFalse: [
              traits string _AddSlots: (|
                whiteSpace = ' \r\n\t' |) ].
            log: 'Patched traits string whiteSpace'.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (8080)'
        
         port <- 8080.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         registerForAutomaticStartup = ( |
            | 
            snapshotAction
              forCommandLineArg: '-http-port'
                       DoAction: (| parent* = lobby.
                                    value: i With: arg = (
                                     webserver startOn: 
                                        (snapshotAction commandLine at: i succ) asInteger. 
                                     i +2).
                                 |)).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         registerServlet: s = ( |
            | servlet: s. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: request and response\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         request = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver request.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (dictionary copyRemoveAll)'
        
         headerFields <- dictionary copyRemoveAll.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         method <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver request p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         headerFieldAt: k IfAbsent: blk = ( |
            | headerFields at: k IfAbsent: blk).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         headerFieldsAt: k Put: v = ( |
            | headerFields at: k Put: v. self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readHeader: io IfFail: blk = ( |
             line.
             log.
             methodLine.
             readLog.
             req.
            | 
            log: [|:m| webserver log: m ].
            readLog: [|l| l: (io readLineIfFail: [^ blk value]) shrinkwrapped. log value: l. l].
            readMethodLine: readLog value IfFail: [^ blk value].
            [line: readLog value. line size > 0] whileTrue: [
              readHeaderLine: line IfFail: [^ blk value]].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readHeaderLine: line IfFail: blk = ( |
            | 
            line findSubstring: ':'
                     IfPresent: [|:i| headerFieldsAt: (line copyFrom: 0 UpTo: i)
                                                 Put: (line copyFrom: i + 1 UpTo: line size) ]
                      IfAbsent: [^ blk value]. "Malformed header"
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         readMethodLine: line IfFail: blk = ( |
             methodLine.
            | 
            methodLine: line asTokensSeparatedByWhiteSpace. 
            method: (methodLine at: 0 IfAbsent: [^ blk value]).
            url:    (methodLine at: 1 IfAbsent: [^ blk value]).
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'request' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         url <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: request and response\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         response = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver response.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'text/html; charset=UTF-8\')'
        
         contentType <- 'text/html; charset=UTF-8'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'\')'
        
         contents <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver response p.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         contentsLength = ( |
            | 
            contents size asString).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         guessContentTypeFromName: n = ( |
             x.
            | 
            x: (n asTokensSeparatedByCharactersIn: '.') last.
            x = 'html' ifTrue: [contentType: 'text/html; charset=UTF-8'     ].
            x = 'js'   ifTrue: [contentType: 'application/ecmascript'       ].
            x = 'css'  ifTrue: [contentType: 'text/css'                     ].
            x = 'gif'  ifTrue: [contentType: 'image/gif'                    ].
            x = 'jpg'  ifTrue: [contentType: 'image/jpeg'                   ].
            x = 'png'  ifTrue: [contentType: 'image/png'                    ].
            x = 'pdf'  ifTrue: [contentType: 'application/pdf'              ].
            x = 'gz'   ifTrue: [contentType: 'application/x-compressed'     ].
            x = 'zip'  ifTrue: [contentType: 'application/zip'              ].
            x = 'dmg'  ifTrue: [contentType: 'application/x-apple-diskimage'].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         p* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         writeBodyOn: io IfFail: blk = ( |
            | 
            io write: contents IfFail: [^ blk value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> 'p' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         writeHeaderOn: io IfFail: blk = ( |
             broken.
            | 
            broken: [^ blk value].
            io write: 'HTTP/1.0 ', statusCode, '\n'            IfFail: broken.
            io write: 'Content-Type: ', contentType, '\n'      IfFail: broken.
            io write: 'Content-Length: ', contentsLength, '\n' IfFail: broken.
            io write: 'Connection: close', '\n'                  IfFail: broken.
            io write: '\n' IfFail: broken.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'response' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: InitializeToExpression: (\'200 OK\')'
        
         statusCode <- '200 OK'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         runServerLoop = ( |
            | 
            [| io <- unixGlobals os_file. |
            serverSocket initialize: 'server socket'.
                io: serverSocket acceptConnection.
                (message copy receiver: self 
                              Selector: 'handleRequest:' 
                                  With: io) fork resume.
            ] loop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fComment: TODO: Replace with timeout call of message.\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         safeHandle: req IfFail: blk = ( |
             k.
             p.
             w.
            | 
            k: (| p* = traits oddball.
                kill: ps After: ms = (
                 process this sleep: ms.
                 ps abortIfLive.
                 self )
               |).
            p: (message copy receiver: servlet 
                             Selector: 'handle:' 
                                 With: req) fork resume.
            w: (message copy receiver: k 
                             Selector: 'kill:After:' 
                                 With: p
                                 With: servletTimeout) fork resume.
            p waitForDeath returnValue
               ifNil: [^ blk value]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (webserver deadProcess)'
        
         serverProcess <- bootstrap stub -> 'globals' -> 'webserver' -> 'deadProcess' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (os_file deadCopy)'
        
         serverSocket <- os_file deadCopy.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         servlet <- bootstrap stub -> 'globals' -> 'miocontrol' -> 'servlet' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: internal state\x7fModuleInfo: Module: webserver InitialContents: InitializeToExpression: (30000)'
        
         servletTimeout <- 30000.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         start = ( |
            | 
            serverProcess isAlive ifTrue: [stop].
            (message copy receiver: self Selector: 'startServerOn:' With: port) fork.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         startOn: aPort = ( |
            | port: aPort. start).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: webserver InitialContents: FollowSlot'
        
         startServerOn: port = ( |
            | 
            initialiseSocketOn: port.
            runServerLoop.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         stop = ( |
            | 
            serverSocket closeIfFail: [
              log: 'Could not close serverSocket'].
            serverProcess abort.
            serverProcess: deadProcess.
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         = x = ( |
            | == x).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'identity' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         hash = ( |
            | identityHash).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'oddball' -> () From: ( | {
         'Comment: return the receiver, not a copy\x7fModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: public'
        
         copy = ( |
            | self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'mixins' -> 'unordered' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         descendantResponsibilities = bootstrap setObjectAnnotationOf: bootstrap stub -> 'mixins' -> 'unordered' -> 'descendantResponsibilities' -> () From: ( |
             {} = 'Comment: The following methods must be implemented by a descendant.\x7fModuleInfo: Creator: mixins unordered descendantResponsibilities.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'traits' -> 'oddball' -> () From: ( | {
         'ModuleInfo: Module: webserver InitialContents: FollowSlot\x7fVisibility: private'
        
         parent* = bootstrap stub -> 'lobby' -> ().
        } | ) 



 '-- Side effects'

 globals modules webserver postFileIn
