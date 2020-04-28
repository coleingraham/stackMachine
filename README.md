# stackMachine
A rough implementation of a simple stack machine and some benchmarking to
compare it to pure Haskell and Lua.

This was made by following [this](https://www.youtube.com/watch?v=OjaAToVkoTw)
video. The core implementation is roughly what is talked about, however there
are a few variants for testing the differences in performance. The main
executable simply runs benchmarks via
[criterion](https://hackage.haskell.org/package/criterion) (use
`--output benchmarks.html` to get the results with nice pretty graphs) comparing
the following versions of factorial that prints the result for several arguments:

* a pure Haskell implementation
* two versions of this stack machine with some minor internal differences
* three flavors of pure Lua implementation
    * one that creates the 'LuaState' each time
    * one that uses the same 'LuaState' but still loads the source each time
    * one that already has a 'LuaState' with the code and only executes it
