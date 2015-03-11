 '0.2.0'
 '
Copyright 2014-2015 AUTHORS.
See the legal/LICENSE file for license information and legal/AUTHORS for authors.
'

modules webserver version >= (modules init moduleVersion copyOn: '0.2.0') ifFalse: [^ error: 'Need at least version 0.2.0 of modules webserver']


 '-- Module body'

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         lobbyBrowserServlet = bootstrap define: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () ToBe: bootstrap addSlotsTo: (
             bootstrap remove: 'directory' From:
             bootstrap remove: 'fileInTimeString' From:
             bootstrap remove: 'myComment' From:
             bootstrap remove: 'postFileIn' From:
             bootstrap remove: 'preFileIn' From:
             bootstrap remove: 'revision' From:
             bootstrap remove: 'subpartNames' From:
             bootstrap remove: 'tree' From:
             globals modules init copy ) From: bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals modules lobbyBrowserServlet.

CopyDowns:
globals modules init. copy 
SlotsToOmit: directory fileInTimeString myComment postFileIn preFileIn revision subpartNames tree.

\x7fIsComplete: '.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot\x7fVisibility: public'
        
         directory <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: InitializeToExpression: (_CurrentTimeString)\x7fVisibility: public'
        
         fileInTimeString <- _CurrentTimeString.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         myComment <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         postFileIn = ( |
            | resend.postFileIn).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         preFileIn = ( |
            | 
            modules webserver version >= (modules init moduleVersion copyOn: '0.2.0')
              ifFalse: [error: 'Need at least version 0.2.0 of modules webserver']).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot\x7fVisibility: public'
        
         revision <- '0.1.0'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot\x7fVisibility: private'
        
         subpartNames <- ''.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'modules' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: InitializeToExpression: (\'org_selflanguage_webserver\')'
        
         tree <- 'org_selflanguage_webserver'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         lobbyBrowserServlet = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( |
             {} = 'ModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         parent* = bootstrap setObjectAnnotationOf: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( |
             {} = 'Comment: An in-browser view of the current Self world\x7fModuleInfo: Creator: globals webserver exampleServlets lobbyBrowserServlet parent.
'.
            | ) .
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         add: s ToCategory: aSequence In: categories = ( |
            | 
            aSequence size > 1 ifTrue: [
             1 to: aSequence size Do: [|:i. p|
               p: aSequence copyFrom: 0 UpTo: i.
               categories
                   findFirst: [|:v. :k| p = k] 
                   IfPresent: true
                    IfAbsent: [categories at: p Put: list copyRemoveAll]]].
            categories
                findFirst: [|:v. :k| aSequence = k] 
                IfPresent: [(categories at: aSequence) add: s]
                 IfAbsent: [categories at: aSequence Put: (list copyRemoveAll add: s)].
            self).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: html for slot\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         appropriateSigilFor: slot = ( |
            | 
            slot value isReflecteeAssignment ifTrue: [^ ' &larr; ']. ' = ').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         banner = ( |
            | 
            '<h1><small>- live browsing of Self object memory -</small></h1>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         breadcrumbsFor: m = ( |
            | 
            m creatorPathIfPresent: [|:p. i <- 0. t. s <- '<a href="/">lobby</a> ' |
                [i < p size] whileTrue: [
                  t: p clone contents: p contents slice: 0 @ i.
                  s: s, '<a href="/', (transform: t fullName), '/">', t shortName, '</a> &rarr; '.
                  i: i + 1
                ].
                s]
            IfAbsent: '').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         buildHtmlForSlotsIn: categories StartingWith: prefix = ( |
            | 
            '<div class="row">
              <div class="col-md-12">
                <h3>
                  <small>
                    <span class="glyphicon glyphicon-chevron-down"></span>
                  </small>', 
                  (prefix size > 0 ifTrue: [prefix last] False: ''), 
            '   </h3>
              </div>
              <div class="col-md-12">
                <p>
                  <table class="table table-condensed table-hover">',
                  [| slots. html <- '' |
                    slots: categories at: prefix IfAbsent: [sequence copyRemoveAll].
                    slots: (slots copy filterBy: [|:s| s isParent]) copySort, 
                           (slots copy filterBy: [|:s| s isParent not]) copySort.
                    slots do: [|:s| 
                      s isAssignment ifFalse: [html: html, htmlForSlot: s]].
                    html
                  ] value,
            '     </table>
                </p>',
                  [| cat. html <- '' |
                    cat: (categories keys copy
                       filterBy: [|:e| (e size = (prefix size + 1)) && [prefix = (e slice: 0 @ prefix size)]]).
                    cat copySort do: [|:n| 
                       html: html, '<div class="col-md-12">', 
                              (buildHtmlForSlotsIn: categories StartingWith: n), 
                               '</div>'].
                    html
                  ] value,
            '  </div>
            </div>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         buildSlots: o = ( |
             categories.
            | 
            categories: dictionary copyRemoveAll.
            o do: [|:s. c| 
               c: ('\x7f' split: s categoryIfFail: '') asSequence.
               (c lastIfAbsent: '') = '\x7f' ifTrue: [c: c copyWithoutLast].
               add: s ToCategory: c In: categories].
            buildHtmlForSlotsIn: categories StartingWith: sequence copy).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: contents for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         contentsForMethod: m = ( |
            | 
            wrapCode: rawContentsForMethod: m).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: contents for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         contentsForObject: o = ( |
            | 
            o isReflecteeMethod ifTrue: [contentsForMethod: o] False: [escape: o safeName]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: support\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         escape: s = ( |
             codes.
             escapees.
             r <- ''.
            | 

            escapees: '<>&'.
            codes: ('&lt;' & '&gt;' & '&amp;') asVector.
            s do: [| :c | 
                (escapees includes: c) ifTrue: [
                    r: r, (codes at: (escapees keyAt: c)).
                ] False: [
                    r: r, c.
                ].
            ].
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         footer = '</div></body></html>'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         getMirrorAtPath: p = ( |
            | 
            getMirrorAtPath: p StartingAt: reflect: lobby).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         getMirrorAtPath: p StartingAt: m = ( |
             o.
            | 
            o: m. p do: [|:n| o:(o at: n IfAbsent: [^ o]) contents]. o).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         handle: u = ( |
             o.
             r.
             t.
            | 
            u req url = '/' 
              ifTrue: [o: reflect: lobby]
              False: [
                o: getMirrorAtPath: 
                  u req url asTokensSeparatedByCharactersSatisfying: [|:c | c = '/']].
            u res write: htmlFor: o.
            u).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         header = '
<!DOCTYPE HTML>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>Self WWW Browser</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<!-- Bootstrap styles -->
<link rel=\"stylesheet\" href=\"//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css\">
<script src=\"//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js\"></script>
<!-- Generic page styles -->
<link rel=\"stylesheet\" href=\"/css/style.css\">
</head>
<body>
<div class=\"container\">
'.
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         htmlFor: o = ( |
             r.
            | 

            r: header.
            r: r, banner.
            r: r, '<h1><small>'.
            r: r, breadcrumbsFor: o.
            r: r, '</small>', (o evalName), '</h1>'.
            r: r, '<div class="row"><div class="col-md-12">'.
            o comment != '' ifTrue: [
             r: r, '<p><pre>', (o comment), '</pre></p>'].
            r: r, '<p>CopyDowns: ', (o copyDowns printString), '</p>'.
            r: r, '<p>CreatorPath: ', (' ' join: o creatorPath), '</p>'.
            r: r, '<p>', (o moduleSummaryStringForSlotsFilteredBy: true), '</p>'.
            r: r, '</div></div>'.
            r: r, buildSlots: o.
            r: r, footer.
            r).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: html for slot\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         htmlForSlot: s = ( |
            | 
            '<tr><td width=60%>',
            (linkForObject: s),
            (appropriateSigilFor: s),
            (contentsForObject: s value),
            '</td><td width=40%>',
            (s comment = '' ifTrue: '' False: ['<em>', s comment, '</em>']), 
            '</td></tr>').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: link for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         linkForObject: s = ( |
            | 
            s value creatorPathIfPresent: [|:p|
              '<a href="/', (transform: p fullName), '/">', s fullName, '</a>']
            IfAbsent: [s fullName]).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'ModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         parent* = bootstrap stub -> 'traits' -> 'clonable' -> ().
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: contents for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         rawContentsForMethod: m = ( |
            | escape: (selfMethodText copyForMethod: m value) asSlotContents asString shrinkwrapped).
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: link for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         transform: s = ( |
            | 
            s replace: ' ' With: '/').
        } | ) 

 bootstrap addSlotsTo: bootstrap stub -> 'globals' -> 'webserver' -> 'exampleServlets' -> 'lobbyBrowserServlet' -> 'parent' -> () From: ( | {
         'Category: contents for object\x7fModuleInfo: Module: lobbyBrowserServlet InitialContents: FollowSlot'
        
         wrapCode: c = ( |
            | [|:t | '<', t, '>', c, '</', t, '>'] value: ((c includes: '\n') ifTrue: 'pre' False: 'code')).
        } | ) 



 '-- Side effects'

 globals modules lobbyBrowserServlet postFileIn
