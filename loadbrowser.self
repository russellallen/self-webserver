[| s |
  "
    This is an example setup script to load the lobby browser website,  
    setup the webserver and save to a snapshot ready to run.  
  "
  s: webserver exampleServlets lobbyBrowserServlet copy.
  webserver server registerServlet: s.
  webserver server port: 8000.
  webserver server startAutomatically: true.
  shell saveAs: 'browser.snap'.
  shell quitNoSave.
] value

