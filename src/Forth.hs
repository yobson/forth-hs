{-
 - Copyright (c) 2026 James Hobson
 -
 - This file is part of hforth and is distributed
 - under the terms of the GNU General Public License
 - version 3 or (at your option) any later version.
-}

{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, StandaloneDeriving, TypeApplications, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE TupleSections, RecursiveDo, ViewPatterns, GeneralizedNewtypeDeriving, DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, MultiParamTypeClasses #-}
module Forth
( Val
, castE
, push
, pop
, popE
, pushDict
, LiftW(..)
, liftW
, Dict
, parseTokens
, executeForthStack
, compileForth
, runForth
, runForthT
, execForth
, loadForth
, addWords
, libPath
, stackAssert
, getStack
) where

import Data.Typeable
import Data.Map (Map)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Functor
import qualified Data.Map.Lazy as Map
import Data.Text (Text, pack, unpack)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Bifunctor
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (toLower)
import Control.Monad.Fix
import Control.Monad
import Data.Function.Reverse
import Data.Coerce

import Paths_forth_hs

import qualified Data.Vector as V

import Debug.Trace

data Val = forall a . (Typeable a, Show a) => Val a

instance Show Val where
  show (Val x) = show x

type Stack = [Val]

type FWord m = ForthT m ()

type Ident = Text

type Env m = (Stack, [Token], Dict m)

newtype ForthT m a = ForthT (StateT (Env m) (ExceptT String m) a)
  deriving (MonadFix, MonadState (Env m), MonadError String, Functor, Applicative, Monad)

deriving instance MonadIO m => MonadIO (ForthT m)

type Forth = ForthT Identity

modifyStack :: (Monad m) => (Stack -> Stack) -> ForthT m ()
modifyStack f = modify (\(s,p,d) -> (f s,p,d))

modifyProg :: Monad m => ([Token] -> [Token]) -> ForthT m ()
modifyProg f = modify (\(s,p,d) -> (s,f p,d))

modifyDict :: Monad m => (Dict m -> Dict m) -> ForthT m ()
modifyDict f = modify (\(s,p,d) -> (s,p,f d))

getsStack, getStack :: Monad m => ForthT m Stack
getsStack = gets (\(s,_,_) -> s)
getStack = getsStack

getsProg :: Monad m => ForthT m [Token]
getsProg = gets (\(_,p,_) -> p)

getsDict :: Monad m => ForthT m (Dict m)
getsDict = gets (\(_,_,d) -> d)

push :: (Show a, Typeable a, Monad m) => a -> ForthT m ()
push x = modifyStack (Val x :)

pushDict :: Monad m => Token -> ForthT m ()
pushDict x = modifyProg (x :)

getDict :: Monad m => ForthT m [Token]
getDict = do
  dict <- getsProg
  modifyProg (const [])
  pure dict

pop :: Monad m => ForthT m Val
pop = getsStack >>= \case
  [] -> throwError "Empty Stack"
  (x:xs) -> modifyStack (const xs) >> pure x

castE :: (Monad m, Typeable a) => Val -> ForthT m a
castE (Val x) = case cast x of
  Just x'  -> pure x'
  Nothing -> throwError "Type Missmatch"

popE :: (Typeable a, Monad m) => ForthT m a
popE = pop >>= castE

class LiftW a where
  liftWRev :: Monad m => a -> FWord m

instance (Show a, Typeable a, IsFun a ~ False) => LiftW a where
  liftWRev = push

instance {-# OVERLAPPING #-} (Typeable a, LiftW r) => LiftW (a -> r) where
  liftWRev f = do
    x <- popE @a
    liftWRev (f x)

liftW :: (LiftW (ReversedArgs a), Monad m, IsFun a ~ True, ReverseArgs (BoxResult a), Coercible a (BoxResult a), Coercible (ReversedArgs a) (BoxResult (ReversedArgs a)), ReversedArgs (BoxResult a) ~ BoxResult (ReversedArgs a)) => a -> ForthT m ()
liftW = liftWRev . reverseArgs

type Dict m = Map Ident (FWord m, Bool)

data Token = Number Integer
           | String String
           | Suspend Ident
           | Execute
           | W Ident
           | Function Ident [Token] Bool
           | Postpone Token
           | DropIf
           | Drop
           | AnyVal Val
           deriving Show

lexeme :: Parser a -> Parser a
lexeme p = p <* many (space <|> (comment $> ' '))

comment :: Parser String
comment = "(" *> many (satisfy (`notElem` [')'])) <* ")"

stringTok :: Parser String
stringTok = "\"" *> many (satisfy (`notElem` ['"'])) <* lexeme "\""

token :: Parser Token
token =  Number   <$> lexeme decimal
     <|> String   <$> stringTok
     <|> Suspend  <$> ("'" *> word)
     <|> Execute  <$  lexeme (asciiCI "EXECUTE")
     <|> DropIf <$  lexeme (asciiCI "?BRANCH")
     <|> Drop   <$  lexeme (asciiCI "BRANCH")
     <|> Postpone <$> (lexeme (asciiCI "POSTPONE") *> token)
     <|> W        <$> word
     <|> liftA3 Function (lexeme ":" *> word) (tokens <* lexeme ";") (lexeme (asciiCI "IMMEDIATE") $> True)
     <|> liftA3 Function (lexeme ":" *> word) (tokens <* lexeme ";") (pure False)

tokens :: Parser [Token]
tokens = many token

word :: Parser Ident
word = lexeme $ pack <$> some (satisfy (`notElem` [' ', '\n', '\t', ':', ';', '\'', '"', '`']))

parseTokens :: Monad m => Text -> ForthT m [Token]
parseTokens = either throwError pure . parseOnly (skipSpace *> tokens)

stepForth :: Monad m => Token -> ForthT m ()
stepForth (Number i) = push i
stepForth (String s) = push s
stepForth (W i)  = getsDict >>= \d -> case Map.lookup (T.map toLower i) d of
  Just (w,False) -> w
  Just (_,True) -> throwError $ unpack i <> " is IMMEDIATE so can only be used in word definitions"
  Nothing -> throwError $ unpack i <> " not found"
stepForth (Suspend i) = push i
stepForth Execute = popE >>= stepForth . W
stepForth (Postpone t) = pushDict t
stepForth (AnyVal (Val t)) = push t
stepForth t = error $ "Impossible! Got: " <> show t

executeForthStack :: MonadFix m => [Token] -> ForthT m ()
executeForthStack ts = executeForthStack' (V.fromList ts) 0

getInt :: Monad m => Token -> ForthT m Int
getInt (Number i) | i >= 0 = pure $ fromIntegral (i + 1)
getInt (Number i) = pure $ fromIntegral i
getInt (AnyVal v) = castE v >>= getInt . Number
getInt _ = throwError "Not Number"

executeForthStack' :: MonadFix m => V.Vector Token -> Int -> ForthT m ()
executeForthStack' v ((v V.!?) -> Nothing) = pure ()
executeForthStack' v idx@((v V.!?) -> Just (Function i ts im)) = mdo
  env <- Map.insert (T.map toLower i) (prog, im) <$> getsDict
  modifyDict (const env)
  prog <- compileForth ts
  executeForthStack' v (idx + 1)
executeForthStack' v idx@((v V.!?) -> Just DropIf) = do
  test <- popE
  offset <- maybe (throwError "No Branch offset") pure $ v V.!? (idx + 1)
  getInt offset >>= executeForthStack' v . (if test then const (idx + 2) else (idx +))
executeForthStack' v idx@((v V.!?) -> Just Drop) = do
  offset <- maybe (throwError "No Branch offset") pure $ v V.!? (idx + 1)
  getInt offset >>= executeForthStack' v . (idx +)
executeForthStack' v idx@((v V.!?) -> Just x) = stepForth x >> executeForthStack' v (idx + 1)
executeForthStack' _ _ = error "Impossible"

compileToken :: MonadFix m => Token -> ForthT m ()
compileToken (W i) = getsDict >>= \d -> case Map.lookup (T.map toLower i) d of
  Just (w, True)  -> w
  Just (_, False) -> pushDict (W i)
  Nothing -> throwError $ unpack i <> " not found"
compileToken x = pushDict x

compileForth :: MonadFix m => [Token] -> ForthT m (ForthT m ())
compileForth ts = do
  mapM_ compileToken ts
  prog <- reverse <$> getsProg
  trace (show prog) $ pure ()
  executeForthStack . reverse <$> getDict

replaceElem :: Int -> a -> [a] -> [a]
replaceElem i x xs = ys ++ x : drop 1 zs
  where (ys,zs) = splitAt i xs

defaultDict :: Monad m => Dict m
defaultDict = Map.fromList $ map (second (,False))
  [ ("+", liftW ((+) @Integer))
  , ("-", liftW ((-) @Integer))
  , ("=", liftW ((==) @Integer))
  , ("dup", do { (Val x) <- pop ; push x ; push x} )
  , ("swap", do {(Val a) <- pop ; (Val b) <- pop ; push a ; push b})
  , ("over", do {(Val a) <- pop ; (Val b) <- pop ; push b ; push a ; push b})
  , (",",  popE >>= pushDict . Number)
  , ("!", setVal)
  , ("here", getsProg >>= push . toInteger . length)
  , ("drop", void pop)
  ]

setVal :: Monad m => ForthT m ()
setVal = do
  addr <- popE @Integer
  x <- popE
  len <- length <$> getsProg
  modifyProg (replaceElem (len - 1 - fromIntegral addr) (Number x))

runForthT :: Monad m => ForthT m a -> m (Either String a)
runForthT (ForthT fs) = runExceptT $ evalStateT fs ([],[], defaultDict)

runForth :: Forth a -> Either String a
runForth = runIdentity . runForthT

execForth :: MonadFix m => Text -> ForthT m ()
execForth = parseTokens >=> executeForthStack 

loadForth :: (MonadFix m, MonadIO m) => FilePath -> ForthT m ()
loadForth = liftIO . T.readFile >=> execForth

addWords :: Monad m => Dict m -> ForthT m ()
addWords d = modifyDict $ Map.union d

libPath :: MonadIO m => m FilePath
libPath = liftIO getDataDir

stackAssert :: Monad m => (Stack -> Bool) -> ForthT m ()
stackAssert p = do
  test <- p <$> getStack
  unless test $ throwError "Stack assertion failed"
  pure ()
