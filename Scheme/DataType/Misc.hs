module Scheme.DataType.Misc (
   SourcePos, MSP, 
   showSourcePos, showMSP,
) where

import Text.Parsec (SourcePos)
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)

showSourcePos :: SourcePos -> String
showSourcePos sp = 
    let (line, column, sourcename) = (sourceLine sp, sourceColumn sp, sourceName sp) 
    in  sourcename ++ " (line " ++ show line ++ ", column " ++ show column ++ ")"


type MSP = Maybe SourcePos
showMSP :: MSP -> String
showMSP Nothing   = ""
showMSP (Just sp) = showSourcePos sp

