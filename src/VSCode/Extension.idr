module VSCode.Extension

import Text.JSGF

%export "javascript:parse"
parse : String -> ParseResult
parse = jsgfParse

%export "javascript:isSuccessful"
isSuccessful : ParseResult -> Bool
isSuccessful (Left _) = False
isSuccessful (Right _) = True

%export "javascript:getErrorMessage"
getErrors : ParseResult -> String
getErrors (Left e) = e 
getErrors (Right _) = [] 

main : IO ()
main = pure ()
