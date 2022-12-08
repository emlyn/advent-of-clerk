# 🎄 Advent of Code, using Clerk

![Build](https://github.com/emlyn/advent-of-clerk/actions/workflows/build.yaml/badge.svg?branch=main)

My attempt at [Advent of Code](https://adventofcode.com)
with [Clerk](https://clerk.vision).

## View notebook output

You can see the notebook visualisation of the code on
[CLERK.garden](https://github.clerk.garden/emlyn/advent-of-clerk).

## Run locally

Clone this repo, make sure you have [Clojure
installed](https://clojure.org/guides/install_clojure), then run:

``` shell
clj -M:nextjournal/clerk nextjournal.clerk/serve! --watch-paths src --port 7878 --browse
```

This will start the Clerk webserver on port 7878 and watch the `src/`
directory for changes and open Clerk in your browser.

Open one of the files there, e.g. `day_01.clj`, make a change and save
it. You should then see these changes reflected in the browser.

<sup><sub>If Clerk Garden is not up to date, [update](https://github.clerk.garden/emlyn/advent-of-clerk?update=1) it.</sub></sup>
