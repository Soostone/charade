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

