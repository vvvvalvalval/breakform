# DEPRECATED 

**Please use the [scope-capture](https://github.com/vvvvalvalval/scope-capture) library instead.**

# breakform

[![Clojars Project](https://img.shields.io/clojars/v/breakform.svg)](https://clojars.org/breakform)

This library eases REPL-based development, by providing macros which help you save and restore the local environment of a piece of code with minimal effort.

Project status: still alpha, expect changes etc. On the other hand, you typically will only use it in your development environment.

## Rationale

This library is designed to support the programming / debugging methodology advocated by Stuart Halloway in this blog post: [_REPL Debugging: No Stacktrace Required_](http://blog.cognitect.com/blog/2017/6/5/repl-debugging-no-stacktrace-required), which consists of: 

1. recreating the environment in which a piece of code runs (using `def`s) 
2. evaluating forms in the code buffer to narrow down the cause of the problem

What the blog post does not mention is that oftentimes, this first step (recreating the local environment) can get very tedious and error-prone; especially when the values of the environment are difficult to fabricate (HTTP requests, database connecitons, etc.), which can be the case for online programs such as web servers, or if you don't have a keen knowledge of the project.

This library alleviates this pain by:

* providing macros that let you save snapshots of the local environment on the fly: `bf.code/spy` (which additionally saves the evaluated wrapped expression - or the resulting error, which you can inspect using `bf.code/saved-value`) and `bf.code/brk` (which acts as a breakpoint, blocking the flow of the program until you choose to release it from the REPL using `bf.code/loose!`, possibly with a value which supersedes the evaluation of the wrapped expression using `bf.code/loose-with!` or `bf.code/loose-with-ex!`)
* providing macros that let you restore these snapshots from the REPL: `bf.code/defsc` (recreates the environment with global vars, i.e by `def`-ing the local names) and `bf.code/letsc` (recreates the environment with locals, i.e by `let`-ing the local names)

## TODO

* Demo video
* Usage documentation
* disable / enable code sites
* clojure.main/repl sub-repl
* experiment with ClojureScript

## License

Copyright © 2016 Valentin Waeselynck and contributors.

Distributed under the MIT license.
