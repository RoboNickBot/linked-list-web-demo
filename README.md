# A Gentle Introduction to Linked Lists

An interactive web-page demonstration of Linked Lists and how they are
stored in memory based (very very closely) on a Java Applet written by
[Michael Goldwasser](http://mathcs.slu.edu/~goldwasser/)

This was written as a SLU Senior Capstone Project for Fall 2014 and is
released under a BSD-style two-clause license (see
[LICENSE](LICENSE)).

It's now finished as far as the assignment goes, so I guess
pull-requests are welcome!  I've added a
[feature backlog](backlog.org) if you want ideas for contribution.

#### Building

You'll need to build the [GHCJS](https://github.com/GHCJS/GHCJS)
(which is kind of a pain right now).  Follow that link for
instructions.

When you have GHCJS, just ```make``` and then install any dependencies
that cabal asks for (oh yeah, you also need cabal-install) and then
```make``` again and it should work.

The files should all end up in a folder called
```linked-list-dist```.  Navigating a web-browser to
```linked-list-dist/index.html``` will give you the demo.

#### Other ways to use it

Since building is a pain, you can also grab a ```.tgz``` from
[releases](https://github.com/RoboNickBot/linked-list-web-demo/releases)
or (for now) go to
[Linked List Demo](http://octalsrc.net/demos/linked_list.html)

#### Code Guide

This is written in Haskell and uses the JQuery and Canvas libraries
from the [GHCJS project](https://github.com/ghcjs) as well as
[Reactive-Banana](https://www.haskell.org/haskellwiki/Reactive-banana)
for some minimal FRP business.

All the JQuery and Canvas things are in ```JS.hs``` and all FRP
happens in ```LinkedListDemo.hs``` (which is also the Main file).
List parsing and Layout generation happens in ```Links.hs```.
