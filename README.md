# gpipe-quake3

[![Build Status](https://travis-ci.org/csabahruska/gpipe-quake3.svg?branch=master)](
  https://travis-ci.org/csabahruska/gpipe-quake3)

> Quake 3 level viewer in Haskell using GPipe.

`PAK0.PK3` file is needed, get it from the Quake3 demo version.

```
$ cabal install
$ gpipe-quake3 q3dm1
```

[Short gifv](http://imgur.com/a/BRTqh)

![Quake III level viewer](https://raw.githubusercontent.com/csabahruska/gpipe-quake3/master/qpipe-quake3.jpg)

## Optional: Stack build

Stack can be used to build the viewer with minimal dependencies:

1. Fetch the latest [stack](http://haskellstack.org/) tool.
2. Build using stack (this will fetch the ghc Haskell compiler if necessary, 
   and all dependencies):

    ```
    $ stack build
    ```

3. Run using stack:

    ```
    $ stack exec gpipe-quake3 -- q3dm1
    ```
