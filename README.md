self-webserver
==============

This project has two aims:

* to be small simple webserver for Self
* to be a testbed and example of how to package Self resources out of the main Self tree

You will need to be running a world built from the Self github repository 'transporter-tree' branch at least after 1 June 2016.

To import this code, run the following from within Self:
    
  bootstrap read: 'webserver'
          InTree: 'org_selflanguage_webserver'
        RootedAt: 'path/to/objects'.

