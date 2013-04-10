charade
=======

Haskell program that fills Heist templates with randomly generated data in
support of rapid UI prototyping.

To see charade in action follow these steps:

1. `git clone https://github.com/Soostone/charade.git`
1. `cd charade`
1. `cabal install`
1. `dist/build/charade/charade`
1. Point your browser at http://localhost:8000
1. Look in templates/index.tpl to see how this example works

Charade lets you prototype a site's HTML/CSS/JavaScript using *exactly* the
Heist template markup that you will have in your final site, without needing
to write any of the backend code to implement your splices.  Every time you
use a tag implemented as a Haskell splice, just include the "fake" attribute
telling charade how to render dummy data for that tag.

Charade decouples the work of designers and backend developers.  A site could
be started by designers who have a good picture of what they want the
front-end interface to look like using just a small amount of knowledge about
splice tags should be structured.  Then developers could be hired to fill in
the back end.  Alternatively, back end developers could start by developing a
site's data model and back end.  Then they could hire designers to build the
look and feel without needing to set up any back end database infrastructure
or give them an application binary.  This allows designers to work in an
environment much closer to what will ultimately be used in production.

The charade application is a snap application that serves Heist templates
and static files.  The charade application allows you to add a "fake"
attribute anywhere in your HTML to generate fake data in the place of that
tag.  For instance, in the following paragraph charade will replace the
\<myParagraph\> tag with a paragraph of lorem ipsum text.  

    <p>
      <myParagraph fake="lorem"/>
    </p>

Charade provides the following built-in primitives for generating dummy
content with the "fake" attribute:

* bool - true/false
* yesno - yes/no
* int \<min\> \<max\> - an integer in the interval [min,max]
* decimal \<min\> \<max\> - a decimal in the interval [min,max]
* list - a list of 5 copies of its child nodes
* list \<count\> - a list of \<count\> copies of its child nodes
* list \<min\> \<max\> - a randomly sized list
* lorem - a paragraph of lorem ipsum text
* lorem \<count\> - a paragraph of lorem ipsum text repeated \<count\> times
* first-name - randomly chosen English first names
* last-name - randomly chosen English last names
* enum - arbitrary enumerations specified in a file

The charade application also requires a file "charade.cfg" in the current
directory for configuration.  The config file should look like this:

    # Select whether random data is generated every request or only once when the
    # app is initialized.  If you specify "dynamic" here, then new random data
    # will be generated every request.  If you specify "static", then random data
    # will be generated once and every request will see the same data.
    mode = "dynamic"
    
    # Directory holding Heist templates
    tdir = "snaplets/heist/templates"
    
    # List of files with custom enumerations for random generation
    enums = ["titles.txt"]
    
    # Directory where static resources are located
    staticDir = "static"
    
    # Route for accessing static data
    staticRoute = ""
