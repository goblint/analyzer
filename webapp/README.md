# About
Web frontend for Goblint.

__Server:__
[node.js](http://nodejs.org/),
[express](http://expressjs.com/),
[jade](http://jade-lang.com/),
[stylus](http://learnboost.github.com/stylus/),
[connect-assets](https://github.com/TrevorBurnham/connect-assets),
[CoffeeScript](http://coffeescript.org/)

__Client:__
[jQuery](http://jquery.com/),
[Twitter Bootstrap](http://twitter.github.com/bootstrap/),
[AngularJS](http://angularjs.org/),
[AngularUI](http://angular-ui.github.io/),
[CodeMirror](http://codemirror.net/)

#Installation
    npm install && bower install

#Start
    coffee server

Alternatively (reloads on changes, needs nodemon):

    nodemon server.coffee

A JS version can be compiled using:

    coffee -c server
