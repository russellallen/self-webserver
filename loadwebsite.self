(
  [|:dir. :snap. s |
    "
      This is an example setup script to load a static website from 
      a local directory, setup the webserver and save to 
      a snapshot ready to run.  
    "
    s: webserver exampleServlets staticSiteServer copy loadTreeFrom: dir.
    webserver server registerServlet: s.
    webserver server port: 8000.
    webserver server startAutomatically: true.
    shell saveAs: snap.
    shell quitNoSave.
  ] value: (_CommandLine at: (_CommandLine keyOf: '-tree' IfAbsent: -2) + 1 IfAbsent: '')
    With:  (_CommandLine at: (_CommandLine keyOf: '-snap' IfAbsent: -2) + 1 IfAbsent: 'site.snap')
)
