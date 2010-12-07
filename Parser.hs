module Parser (
               parseCprbSrc,
               Control(..),
               CppPart(..),
               CppSrc,
               CprbPart(..),
               CprbSrc
) where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec 

data Control = Quoting deriving (Eq,Ord,Show,Read)

controls = [Quoting]

data CppPart = Src String SourcePos SourcePos | 
               OneLineComment String SourcePos SourcePos | 
               Comment String SourcePos SourcePos | 
               Begin Control SourcePos |
               End Control SourcePos
              deriving (Eq,Ord, Show)
--type CppSrc = [(SourcePos, SourcePos, CppPart)]
type CppSrc = [CppPart]

data CprbPart = 
              BareRuby CppSrc |
              HereDocument Control CppSrc
              deriving (Eq,Ord, Show)
type CprbSrc = [CprbPart]


peal::CppPart -> String
peal (Src x _ _) = x
peal (OneLineComment x _ _) = x
peal (Comment x _ _) = x
peal _ = ""



headPosition (Src _ p _) = p
headPosition (OneLineComment _ p _) = p
headPosition (Comment _ p _) = p
headPosition (Begin _ p) = p
headPosition (End _ p) = p

lastPosition (Src _ _ p ) = p
lastPosition (OneLineComment _ _ p) = p
lastPosition (Comment _ _ p) = p
lastPosition (Begin _ p) = p
lastPosition (End _ p) = p




parseCprbSrc::String -> String -> Either ParseError CprbSrc 
parseCprbSrc fn str = 
  case (parse cppSrc fn str) of
       Right cppSrc -> parse cprbSrc fn cppSrc
       Left err -> Left err





cppChar = do
  p <- getPosition
  c <- anyChar
  return $ Src [c] p p           
  
comment = do 
  p1 <- getPosition
  try $ string "/*"
  cs <- manyTill anyChar (try (string "*/"))
  p2 <- getPosition
  return $ Comment cs p1 p2
  
oneLineComment = do 
  p1 <- getPosition
  try $ string "//"
  cs <- manyTill anyChar (try newline)
  p2 <- getPosition
  return $ OneLineComment cs p1 p2

cppSrc::Parser CppSrc
cppSrc = do
  cpp <- many (comment <|> oneLineComment <|> cppChar) 
  return $ detectControl.map reduce.g $ cpp
    where
      reduce::[CppPart] -> CppPart
      reduce parts = case head parts of
                       Src _ _ _-> Src (concat $ map peal parts)
                           (minimum $ map headPosition parts)
                           (maximum $ map lastPosition parts)
                       _ -> head parts
      g = groupBy $ 
          \x y-> case (x,y) of
                   (Src _ _ _, Src _ _ _) -> True
                   _              -> False
                   
      d con char pos
          | char == '"' = [con Quoting pos]
          | True        = []
      cleanse = let 
             c "" = ""
             c ('"':xs) = xs
             c x = x
        in reverse . c . reverse . c
                          
      detectControl = concat. map(
        \x -> case x of             
                Comment str p1 p2
                  | length str == 0 -> [x]
                  | True -> concat [d End (head str) p1, 
                                    [Comment (cleanse str) p1 p2], 
                                    d Begin (last str) p2]
                _ -> [x]
        )
                      
      
              
type CppParser = GenParser CppPart ()                      
cppToken = token showCppPart headPosition 

showCppPart::CppPart -> String
showCppPart part =
  case part of
    Src x _ _            -> abbrev x
    OneLineComment x _ _ -> abbrev $ "//" ++ x
    Comment x _ _        -> abbrev $ "/*" ++ x ++ "*/"
    Begin c _            ->  ("Begin " ++ show c)
    End c _              ->  ("End " ++ show c)
  where
    abbrev str 
      | length str < 60 = str
      | True = (take 28 str) ++ "...." ++ (reverse.take 28.reverse $ str)

begin::Control -> CppParser ()
begin c = cppToken f 
    where
      f (Begin c1 _)
        | c == c1 = Just ()
        | True    = Nothing
      f _ = Nothing

end::Control -> CppParser ()
end c = cppToken f  
    where
      f (End c1 _)
        | c == c1 = Just ()
        | True    = Nothing
      f _ = Nothing

notBeginNorEnd::CppParser CppPart
notBeginNorEnd = cppToken f <?> "non-comment"
    where
      f (Begin _ _) = Nothing
      f (End _ _) = Nothing
      f x = Just x

hereDocument::Control -> CppParser CprbPart
hereDocument c = do
  try (begin c)
  xs <- manyTill notBeginNorEnd (try (end c))
  return $ HereDocument c xs
  
bareRuby::CppParser CprbPart
bareRuby = fmap (BareRuby . return) notBeginNorEnd

cprbSrc::CppParser CprbSrc
cprbSrc = many $ foldr1 (<|>) (map hereDocument controls) <|> bareRuby

  
