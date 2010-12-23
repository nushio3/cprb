#!/usr/bin/env runhaskell

import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.Cmd
import System.IO
import System.IO.Unsafe

import Debug
import Parser


data Flag 
  = OutputFilename String
  | KeepTempFiles 
    deriving (Eq, Ord, Read, Show)

optDescrs :: [OptDescr Flag]
optDescrs =
  [ Option [] ["keep"] (NoArg KeepTempFiles) "keep intermediate files that are generated during\ninternal preprocessing steps." ,
    Option ['o'] ["output-file"] (ReqArg OutputFilename "file") "specify name and location of the output file.\nDefault output filename is input filename with\n'rb' removed from the extension;for example,\n'cprb foo.cpprb' generates 'foo.cpp'"
  ]

parsedArgv :: ([Flag], [String])
parsedArgv = unsafePerformIO $ do
  argv <- getArgs
  case getOpt Permute optDescrs argv of
    (opts, nonOpts, []) 
      | length nonOpts > 0 -> return (opts, nonOpts)
      | True               -> bailout "no input files"
    (_, _, errs) -> bailout $ concat errs

bailout msg = do
  putStrLn $ "Error: " ++ msg
  putStrLn $ ""
  putStrLn $ usageInfo "Usage: cprb [OPTION...] files..." optDescrs
  exitFailure

flags::[Flag]
flags = fst parsedArgv
argv::[String]
argv  = snd parsedArgv



main = forM_ argv treat

treat filename = do
  src <- readFile filename
  naws <- newIORef $ neverAppearingWords src

  case parseCprbSrc filename src of
    Right cprbSrc -> generate filename naws cprbSrc 
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
  
neverAppearingWords src = filter (not . (`isInfixOf` src)) $
                          ["END"++show i| i<-[0..]]
  
generate filename naws cprbSrc = do
  let fns = [filename, fnRuby, fnCpp]
  when (nub fns /= fns) $ bailout $ 
    "filename collision :" ++ concat (intersperse ", " fns)
  ret <- fmap concat $ mapM toRuby cprbSrc
  writeFile fnRuby ret
  system $ "ruby " ++ fnRuby ++ " > " ++ fnCpp
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
        
        makeHereDocument src = do
          (naw:xs) <- readIORef naws
          writeIORef naws xs
          let srclines = lines src
          return $ "<<" ++ naw ++ "\n"
                    ++ (escape . trim) src
                    ++ "\n" ++ naw ++ "\n"
        
        escape = concat . map (\c -> if c == '\\' then "\\\\" else [c])
        trim = let 
          trim1 [] = []
          trim1 list@(x:xs)
            | all isSpace x = xs
            | otherwise = list 
          in concat.intersperse "\n".reverse.trim1.reverse.trim1.lines

        (fnBody, fnExt) = splitExtension filename
        (fnExt1, fnExt2) = splitAt (length fnExt - 2) fnExt

        fnRuby = if KeepTempFiles `elem` flags
                 then fnBody <.> "rb"
                 else "/tmp/cprb.rb"
        ofnCandidate = concat $ flip map flags $
                       \opt -> case opt of
                                 OutputFilename fn -> [fn]
                                 _ -> []
        fnCpp = case ofnCandidate of
                  [] 
                    | fnExt2=="rb" -> fnBody <.> fnExt1
                    | True         -> filename
                  (x:xs) -> x

