{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module VM.PtrVM where

import           Control.DeepSeq
import           Control.Monad
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashTable.IO   as H
import           Data.IORef
import           Data.Monoid
import           Data.String         (IsString)
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics        (Generic)
import           Text.Printf

import           VM.Types

-- |Wrapper type for all values that can be passed to/from or used within
-- a Performance VM.
--
-- In addition to everything that the VM can do internally, this must provide
-- constructors that wrap everything that would need a Lua.StackValue instance
-- when using hslua. This means that first-class Composer data types don't
-- actually need to be converted to anything, only wrapped.
data PerformanceValue
    -- Standard
    = PVNull
    | PVAddr   !Addr -- private
    | PVBool   !Bool
    | PVInt    !Int
    | PVDouble !Double
    | PVString !T.Text
    | PVTable  !(HM.HashMap PerformanceValue PerformanceValue)

--    -- Pitch
--    | PVPitchLetter     !Pitch.Letter
--    | PVPitchAccidental !Pitch.Accidental
--    | PVPitch           !Pitch.Pitch
--    | PVAPitchOctave    !APitch.Octave
--    | PVAPitch          !APitch.AbsolutePitch

    deriving (Show,Eq,Ord,Generic)

instance Hashable PerformanceValue
instance NFData   PerformanceValue

type Stack   = VM.IOVector PerformanceValue
type Globals = H.BasicHashTable Var PerformanceValue -- register

-- |Atomic 'VM' operations with any arguments partially applied.
--
-- This could be made into a Reader in order to make sure that the code and vm
-- are not mutated, but I wouldn't want that to slow anything down.
data Operation = Operation {
         opName       :: T.Text -- ^this is only for debugging so making it not strict should be faster
        ,runOperation :: VM -> IO ()
    }

data VM = VM {
         stackPtr       :: !(Ptr Addr)
        ,framePtr       :: !(Ptr Addr)
        ,instructionPtr :: !(Ptr Addr)
        ,stack          :: !Stack
        ,globals        :: !Globals
        ,debug          :: !Bool
        ,code           :: !(V.Vector Operation)
    } deriving (Generic)

--debugVM :: VM -> IO String
--debugVM vm = do
--    Addr ip <- peek $ instructionPtr vm
--    Addr sp <- peek $ stackPtr       vm
--
--    stk <- V.freeze $ VM.take sp $ stack vm
--    return $ "ip: " ++ show ip ++  " stack " ++ show stk

-- |Complete performances, which are the result of running all supplied
-- 'Operation's in sequence.
data Performance = Performance {
         pMain :: !Addr                 -- ^the index of whatever the "main" instruction is
        ,pCode :: !(V.Vector Operation) -- ^all of the program instructions
    }

newVM
    :: Int -- ^stack size
    -> Int -- ^globals size
    -> Bool -- ^debug flag
    -> IO VM
newVM ss gs dbg = do
    spt <- malloc
    poke spt (-1)
    fpt <- malloc
    poke fpt (-1)
    ipt <- malloc
    poke ipt (-1)
    stk <- VM.new ss
    gbl <- H.newSized gs
    return $ VM spt fpt ipt stk gbl dbg mempty

setPerformance :: Performance -> VM -> IO VM
setPerformance (Performance m c) vm = do
    poke (stackPtr       vm) (-1)
    poke (framePtr       vm) (-1)
    poke (instructionPtr vm) m
    return $ vm { code = c }

setAndRun :: VM -> Performance -> IO VM
setAndRun vm p = do
    vm' <- setPerformance p vm
    runCurrentInstruction vm'
    return vm'

-- |This gives the result of x++
inc :: Ptr Addr -> IO Addr
inc p = do
    x <- peek p
    let !a = x + 1
    poke p a
    return a

-- |This give the result before calling x--
dec :: Ptr Addr -> IO Addr
dec p = do
    x <- peek p
    let !a = x - 1
    poke p a
    return x

debugOperation :: Int -> Operation -> VM -> IO ()
debugOperation i op vm = do
    Addr sp <- peek $ stackPtr       vm
--    !stk <- V.freeze $ VM.take sp $ stack vm
    printf "%d:\t%s\t\n" i (opName op)

runCurrentInstruction :: VM -> IO ()
runCurrentInstruction vm = do
    Addr i <- peek $ instructionPtr vm
    let op = code vm V.! i

--    when (debug vm) $ debugOperation i op vm

    runOperation op vm
{-# INLINE runCurrentInstruction  #-}

-- |Increments the 'instructionPtr' and runs the 'Operation' at the next 'Addr'
-- in the supplied code 'V.Vector'. This is used for most "simple" 'Operation's
-- (e.g. anything other than call or return).
nextInstruction :: VM -> IO ()
nextInstruction vm = do
    void $ inc (instructionPtr vm)
    runCurrentInstruction vm
{-# INLINE nextInstruction  #-}

push :: PerformanceValue -> VM -> IO ()
push x vm = inc (stackPtr vm) >>= \(Addr i) -> VM.write (stack vm) i x
{-# INLINE push  #-}

---- |Pushes two values onto the stack. Possibly useful for unary operations.
--push2 :: PerformanceValue -> PerformanceValue -> VM -> IO ()
--push2 a b vm = do
--    inc spr >>= \(Addr i) -> VM.write (stack vm) i a
--    inc spr >>= \(Addr i) -> VM.write (stack vm) i b
--    where
--        spr = stackPtr vm
--{-# INLINE push2  #-}
--
---- |Pushes three values onto the stack. Useful for binary operations and 'oCall'.
--push3 :: PerformanceValue -> PerformanceValue -> PerformanceValue -> VM -> IO ()
--push3 a b c vm = do
--    inc spr >>= \(Addr i) -> VM.write (stack vm) i a
--    inc spr >>= \(Addr i) -> VM.write (stack vm) i b
--    inc spr >>= \(Addr i) -> VM.write (stack vm) i c
--    where
--        spr = stackPtr vm
--{-# INLINE push3  #-}

-- |Pops one value off of the stack.
pop :: VM -> IO PerformanceValue
pop vm = dec (stackPtr vm) >>= VM.read (stack vm) . unAddr
{-# INLINE pop  #-}

---- |Pops two values off of the stack.
--pop2 :: VM -> IO [PerformanceValue]
--pop2 vm = do
--    Addr spt <- peek spr
--    modifyIORef' spr (\x -> x - 2)
--    a <- VM.read (stack vm) spt
--    b <- VM.read (stack vm) (spt - 1)
--    return [b,a]
--    where
--        spr = stackPtr vm
--{-# INLINE pop2  #-}
--
---- |Pops three values off of the stack.
--pop3 :: VM -> IO [PerformanceValue]
--pop3 vm = do
--    Addr spt <- peek spr
--    modifyIORef' spr (\x -> x - 3)
--    a <- VM.read (stack vm) spt
--    b <- VM.read (stack vm) (spt - 1)
--    c <- VM.read (stack vm) (spt - 2)
--    return [c,b,a]
--    where
--        spr = stackPtr vm
--{-# INLINE pop3  #-}

-- |Helper for following the provided 'Operation' with 'nextInstruction'. Useful
-- for completely sequential functions.
step
    :: T.Text
    -> (VM -> IO ())
    -> Operation -- ^the input followed by 'nextInstruction'
step n f = Operation n $ \vm -> do
    f vm
    nextInstruction vm
{-# INLINE step  #-}

-- |Pushes a single value onto the stack.
oPush :: PerformanceValue -> Operation
oPush x = step nom $ push x
    where
        nom = "PUSH \t(" <> T.pack (show x) <> ")"
{-# INLINE oPush  #-}

oPopThen :: T.Text -> (VM -> PerformanceValue -> IO ()) -> Operation
oPopThen n f = step n $ \vm -> pop vm >>= f vm
{-# INLINE oPopThen  #-}

-- |Pops the value at the top of the stack and prints it.
oPrint :: Operation
oPrint = oPopThen "PRINT" $ const print
{-# INLINE oPrint  #-}

--oPop2Then :: T.Text -> (VM -> [PerformanceValue] -> IO ()) -> Operation
--oPop2Then n f = step n $ \vm -> pop2 vm >>= f vm
--{-# INLINE oPop2Then  #-}

oIOp :: T.Text -> (Int -> Int -> Int) -> Operation
oIOp n f = step n $ \vm -> do
    PVInt b <- pop vm
    PVInt a <- pop vm
    push (PVInt $ f a b) vm -- this does infact need to be @b@ then @a@!
{-# INLINE oIOp  #-}

oISub :: Operation
oISub = oIOp "ISUB" (-)
{-# INLINE oISub  #-}

oIMul :: Operation
oIMul = oIOp "IMUL" (*)
{-# INLINE oIMul  #-}

oEqOp
    :: T.Text
    -> (PerformanceValue -> PerformanceValue -> Bool)
    -> Operation
oEqOp n f = step n $ \vm -> do
    b <- pop vm
    a <- pop vm
    push (PVBool $ f a b) vm -- this does infact need to be @b@ then @a@!
{-# INLINE oEqOp  #-}

oEq :: Operation
oEq = oEqOp "EQ" (==)
{-# INLINE oEq  #-}

oLT :: Operation
oLT = oEqOp "LT" (<)
{-# INLINE oLT  #-}

oLoad :: Addr -> Operation
oLoad offset = step nom$ \vm -> do
    fp <- peek $ framePtr vm
    x  <- VM.read (stack vm) (unAddr $ fp + offset)
    push x vm
    where
        nom = "LOAD \t(" <> T.pack (show $ unAddr offset) <> ")"
{-# INLINE oLoad  #-}

branch :: Addr -> VM -> IO ()
branch a vm = do
    poke (instructionPtr vm) a
    runCurrentInstruction vm
{-# INLINE branch  #-}

-- |Sets the instructionPtr and runs the operation at that address.
oBranch :: Addr -> Operation
oBranch a = Operation nom $ \vm -> do
    poke (instructionPtr vm) a
    runCurrentInstruction vm
    where
        nom = "BR \t(" <> T.pack (show (unAddr a)) <> ")"
{-# INLINE oBranch  #-}

oBranchIfTrue :: Addr -> Operation
oBranchIfTrue a = Operation nom $ \vm -> pop vm >>= f vm
    where
        f vm (PVBool x) = if x
                          then branch a vm
                          else nextInstruction vm
        f vm x = error $ "expected Bool, got " ++ show x
        nom = "BRT \t(" <> T.pack (show (unAddr a)) <> ")"
{-# INLINE oBranchIfTrue  #-}

-- |This is a copy/paste of 'oBranchIfTrue' in order to make sure that
-- 'oBranchIfTrue' doesn't need to pay for the call to 'not' that this does.
-- Otherwise 'oBranchIfTrue' would require passing in 'id' and I don't want to
-- pay for anything that I don't absolutely need to.
oBranchIfFalse :: Addr -> Operation
oBranchIfFalse a = Operation nom $ \vm -> pop vm >>= f vm
    where
        f vm (PVBool x) = if x
                          then nextInstruction vm
                          else branch a vm
        f vm x = error $ "expected Bool, got " ++ show x
        nom = "BRF \t(" <> T.pack (show (unAddr a)) <> ")"
{-# INLINE oBranchIfFalse  #-}

oGStore :: Var -> Operation
oGStore v = step nom $ \vm -> do
    x <- pop vm
    H.insert (globals vm) v x
    where
        nom = "GSTORE " <> T.pack (show v)

oGLoad :: Var -> Operation
oGLoad v = step nom $ \vm -> do
    (Just x) <- H.lookup (globals vm) v
    push x vm
    where
        nom = "GLOAD " <> T.pack (show v)

oCall :: Addr -> Int -> Operation
oCall a args = Operation nom $ \vm -> do
    fp <- peek $ framePtr       vm
    ip <- peek $ instructionPtr vm

    push (PVInt args)      vm -- save the number of arguments
    push (PVAddr fp)       vm -- save framePtr
    push (PVAddr $ ip + 1) vm -- push the return address

    sp <- peek $ stackPtr vm
    -- sets the return address
    poke (framePtr       vm) sp
    -- set the instructionPtr to the provided Addr
    poke (instructionPtr vm) a
    runCurrentInstruction vm
    where
        nom = "CALL \t("
           <> T.pack (show $ unAddr a)
           <> ", "
           <> T.pack (show args)
           <> ")"
{-# INLINE oCall  #-}

oRet :: Operation
oRet = Operation "RET" $ \vm -> do
    r <- pop vm -- get the return value

    -- set the stackPtr to the framePtr
    peek (framePtr vm) >>= poke (stackPtr vm)

    -- |pop everything from CALL off the stack
    PVAddr ip   <- pop vm
    PVAddr fp   <- pop vm
    PVInt nargs <- pop vm
    poke   (instructionPtr vm) ip
    poke   (framePtr       vm) fp

    peek (stackPtr vm) >>= \a -> poke (stackPtr vm) (a - Addr nargs)

    push r vm -- push the return value
    runCurrentInstruction vm
{-# INLINE oRet  #-}

-- |Stops the program.
oHalt :: Operation
oHalt = Operation "HALT" $ const $ return ()
{-# INLINE oHalt  #-}

-- |Defunctionalization of the 'Operation' functions.
data Op
    = Push !PerformanceValue
    | Print
    | Load !Addr          -- ^load a local or arg by relative offset
    | Halt                -- ^stops execution

    | LStore !Addr -- ^store a local variable
    | LLoad  !Addr -- ^pushes the local value at 'Var' to the stack

    | GStore !Var -- ^store a global variable
    | GLoad  !Var -- ^pushes the global value at 'Var' to the stack

    | ISub
    | IMul

    | VLT

    -- Branching
    | Branch        !Addr -- ^unconditional jump to the 'Addr'
    | BranchIfTrue  !Addr -- ^jump if the top of the stack is 'True'
    | BranchIfFalse !Addr -- ^jump if the top of the stack is 'False

    -- Complex
    | Call !Addr !Int     -- ^target function 'Addr' and number of arguments
    | Return
    deriving (Show,Eq)

toOperation :: Op -> Operation
toOperation (Push x)          = oPush x
toOperation Print             = oPrint
toOperation (Load          a) = oLoad a
toOperation ISub              = oISub
toOperation IMul              = oIMul
toOperation VLT               = oLT
toOperation (Branch        a) = oBranch a
toOperation (BranchIfTrue  a) = oBranchIfTrue a
toOperation (BranchIfFalse a) = oBranchIfFalse a
toOperation (Call a b)        = oCall a b
toOperation (GStore v)        = oGStore v
toOperation (GLoad v)         = oGLoad v
toOperation Return            = oRet
toOperation Halt              = oHalt

runPerformance
    :: Int
    -> Int
    -> Bool
    -> Performance
    -> IO VM -- ^the 'VM' in whatever state it is after the 'Performance' is executed
runPerformance ss gs dbg p = do
    vm <- newVM ss gs dbg
    setAndRun vm p

-- Testing

testPerformance :: Performance
testPerformance = Performance 0 $ V.fromList $ map toOperation
    [Push (PVInt 1337)  -- 0
    ,Push (PVInt 1)     -- 1
    ,ISub               -- 2
    ,Push (PVBool True) -- 3
    ,BranchIfTrue 7     -- 4
    ,GStore "test"      -- 5
    ,GLoad  "test"      -- 6
    ,Print              -- 7
    ,Halt]              -- 8

runTestPerformance :: IO ()
runTestPerformance = void $ runPerformance 100 100 True testPerformance

factorialPerformance :: Int -> Performance
factorialPerformance n = Performance m $ V.fromList $ map toOperation
    -- FAC
    [Load (-3)        -- 0 load N (the first argument)
    ,Push (PVInt 2)   -- 1
    ,VLT              -- 2
    ,BranchIfFalse 6  -- 3 if N < 2
    ,Push (PVInt 1)   -- 4
    ,Return           -- 5

    ,Load (-3)        -- 6 load N
    ,Load (-3)        -- 7 load N again
    ,Push (PVInt 1)   -- 8 push 1
    ,ISub             -- 9 compute N - 1
    ,Call fact 1      -- 10 call FAC
    ,IMul             -- 11 multiply N by the result
    ,Return           -- 12

    ,Push (PVInt n)   -- 13 MAIN
    ,Call fact 1      -- 14
    ,Print            -- 15
    ,Halt]            -- 16
    where
        fact = 0  -- address of FAC
        m    = 13 -- address of MAIN

runFactorial :: Int -> IO VM
runFactorial n = runPerformance 101 0 False $ factorialPerformance n

fac n = product [1..n]

