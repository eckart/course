{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell Course/FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
--  getArgs :: IO (List Chars)
--  putStrLn :: Chars -> IO ()
main ::
  IO ()
main = do
  args <- getArgs
  case args of
    Nil -> return()
    a :. _ -> run a

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run path =
  do
    (_,c) <- getFile path
    allFileContents <- getFiles $ lines c
    printFiles allFileContents
-- the same using bind directly:
--  printFiles =<< (getFiles =<< (\(_,c) -> return (lines c)) =<< (getFile path))

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles paths = sequence $ getFile <$> paths

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = (\c -> (path, c)) <$> (readFile path) 

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles xs = let pp = (\(p,c) -> printFile p c) <$> xs
                in foldLeft ((=<<).const) (return()) pp 

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path content = do
  putStrLn $ "========================== "++path
  putStrLn content
 




