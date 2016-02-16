[" Load webserver "

  modules init
    registerTree: 'org_selflanguage_webserver'
              At: 'objects'.

  bootstrap read: 'webserver'
          InTree: 'org_selflanguage_webserver'.

] value