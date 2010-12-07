#!/usr/bin/env runhaskell

import Parser
import Control.Monad
import Data.List
import Data.IORef
import System.IO
import System.Exit

main = do
  src <- getContents
  naws <- newIORef $ neverAppearingWords src
  case parseCprbSrc "STDIN" src of
    Right cprbSrc -> generate naws cprbSrc 
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
  
neverAppearingWords src = filter (not . (`isInfixOf` src)) $ ["END"++show i| i<-[0..]]
  
generate naws cprbSrc = do
  ret <- fmap concat $ mapM toRuby cprbSrc
  putStr ret
      where
        toRuby::CprbPart -> IO String
        toRuby (BareRuby cppSrc) = fmap concat $ mapM cpp2br cppSrc 
        toRuby (HereDocument c cppSrc) = case c of
          Quoting -> makeHereDocument $ concat $ map reconstruct cppSrc
  
        cpp2br (OneLineComment s _ _) = return $ s ++ "\n"
        cpp2br (Comment s _ _) = return $ s
        cpp2br _ = return $ ""
  
        reconstruct (Src s _ _) = s
        reconstruct (OneLineComment s _ _) = "//" ++ s ++ "\n"
        reconstruct (Comment s _ _) = "/*" ++ s ++ "*/"
        reconstruct _ = ""
        
        makeHereDocument s = do
          (naw:xs) <- readIORef naws
          writeIORef naws xs
          return $ "<<" ++ naw ++ "\n" ++ s ++ "\n" ++ naw ++ "\n"
