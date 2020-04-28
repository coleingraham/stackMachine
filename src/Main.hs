{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad
import           Criterion.Main
import           Foreign.C.String
import           Scripting.Lua
import           Scripting.Lua.Raw

import qualified VM.PtrVM as P
import qualified VM.UnsafeVM as U
import           VM.VM

--main :: IO ()
--main = void $ U.runFactorialSeq 20

main :: IO ()
main = do
    la <- newstate

    c <- newCString luaFac

    lb <- newstate
    openlibs lb
    void $ loadstring lb luaFac
    call lb 0 0

    -- this allows me to reuse the same VM, not needing to pay for reallocating
    -- memory each run
    pvm <- P.newVM 101 0 False
    let !pf1  = P.factorialPerformance 1
        !pf5  = P.factorialPerformance 5
        !pf10 = P.factorialPerformance 10
        !pf20 = P.factorialPerformance 20

    uvm <- U.newVM 101 0 False
    let !upf1  = U.factorialPerformance 1
        !upf5  = U.factorialPerformance 5
        !upf10 = U.factorialPerformance 10
        !upf20 = U.factorialPerformance 20

    let !upfs1  = U.factorialPerformanceSeq 1
        !upfs5  = U.factorialPerformanceSeq 5
        !upfs10 = U.factorialPerformanceSeq 10
        !upfs20 = U.factorialPerformanceSeq 20

    defaultMain [
        bgroup "performance VM"
            [ bench "1"  $ whnfIO (runFactorial 1)
            , bench "5"  $ whnfIO (runFactorial 5)
            , bench "10" $ whnfIO (runFactorial 10)
            , bench "20" $ whnfIO (runFactorial 20) ]
       ,bgroup "performance PtrVM"
            [ bench "1"  $ whnfIO (P.setAndRun pvm pf1)
            , bench "5"  $ whnfIO (P.setAndRun pvm pf5)
            , bench "10" $ whnfIO (P.setAndRun pvm pf10)
            , bench "20" $ whnfIO (P.setAndRun pvm pf20) ]
       ,bgroup "performance UnsafeVM"
            [ bench "1"  $ whnfIO (U.setAndRun uvm upf1)
            , bench "5"  $ whnfIO (U.setAndRun uvm upf5)
            , bench "10" $ whnfIO (U.setAndRun uvm upf10)
            , bench "20" $ whnfIO (U.setAndRun uvm upf20) ]
       ,bgroup "performance UnsafeVM Seq"
            [ bench "1"  $ whnfIO (U.setAndRun uvm upfs1)
            , bench "5"  $ whnfIO (U.setAndRun uvm upfs5)
            , bench "10" $ whnfIO (U.setAndRun uvm upfs10)
            , bench "20" $ whnfIO (U.setAndRun uvm upfs20) ]
        ,bgroup "pure Haskell"
            [ bench "1"  $ whnfIO (print $ fac 1)
            , bench "5"  $ whnfIO (print $ fac 5)
            , bench "10" $ whnfIO (print $ fac 10)
            , bench "20" $ whnfIO (print $ fac 20) ]
        ,bgroup "Lua: new state every time"
            [ bench "1"  $ whnfIO (luaPrintFac1 1)
            , bench "5"  $ whnfIO (luaPrintFac1 5)
            , bench "10" $ whnfIO (luaPrintFac1 10)
            , bench "20" $ whnfIO (luaPrintFac1 20) ]
        ,bgroup "Lua: shared state, calls loadstring every time"
            [ bench "1"  $ whnfIO (luaPrintFac2 la 1)
            , bench "5"  $ whnfIO (luaPrintFac2 la 5)
            , bench "10" $ whnfIO (luaPrintFac2 la 10)
            , bench "20" $ whnfIO (luaPrintFac2 la 20) ]
        ,bgroup "Lua: shared state, calls c_luaL_loadstring every time"
            [ bench "1"  $ whnfIO (luaPrintFac2a la c 1)
            , bench "5"  $ whnfIO (luaPrintFac2a la c 5)
            , bench "10" $ whnfIO (luaPrintFac2a la c 10)
            , bench "20" $ whnfIO (luaPrintFac2a la c 20) ]
        ,bgroup "Lua: shared state, code already loaded"
            [ bench "1"  $ whnfIO (luaPrintFac3 lb 1)
            , bench "5"  $ whnfIO (luaPrintFac3 lb 5)
            , bench "10" $ whnfIO (luaPrintFac3 lb 10)
            , bench "20" $ whnfIO (luaPrintFac3 lb 20) ]
        ]

luaPrintFac1 :: Double -> IO ()
luaPrintFac1 n = do
    l <- newstate
    luaPrintFac2 l n

luaPrintFac2 :: LuaState -> Double -> IO ()
luaPrintFac2 l n = do
    openlibs l
    void $ loadstring l luaFac
    call l 0 0
    luaPrintFac3 l n

luaPrintFac2a :: LuaState -> CString -> Double -> IO ()
luaPrintFac2a l c n = do
    openlibs l
    void $ c_luaL_loadstring l c
    call l 0 0
    luaPrintFac3 l n

luaPrintFac3 :: LuaState -> Double -> IO ()
luaPrintFac3 l n = do
    getfield l globalsindex "printFac"
    pushnumber l n
    call l 1 0

luaFac :: String
luaFac = unlines
    ["function fac(n)"
    ,"    if (n == 0) then"
    ,"        return 1"
    ,"    else"
    ,"        return n * fac(n - 1)"
    ,"    end"
    ,"end"
    ,"function printFac(n)"
    ,"    print(fac(n))"
    ,"end"]

