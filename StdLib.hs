module StdLib where

standardFunctions :: String
standardFunctions = lengthFunction ++ printStringFunction ++ printBool ++ printIntList


lengthFunction :: String 
lengthFunction = "\
\length(lst) :: [a] -> Int {\
\  var l = 0;\
\  while(!isEmpty(lst)) {\
\      l = l + 1;\
\      lst = lst.tl;\
\  }\
\  return l;\
\}"


printStringFunction :: String
printStringFunction = "\
\printString(str) :: [Char] -> Void {\
\    while(!isEmpty(str)) {\
\      print(str.hd);\
\      str = str.tl;\
\    }\
\  }"

printBool :: String 
printBool = "\
\printBool(bool) :: Bool -> Void {\
\    if(bool){\
\      print(\"True\");\
\    }\
\    else{\
\      print(\"False\");\
\    }\
\  }"

printIntList :: String 
printIntList = "\
\printIntList(l) :: [Int] -> Void {\
\    print('[');\
\    if(!isEmpty(l)) {\
\        print(l.hd);\
\        l = l.tl;\
\    }\
\    while(!isEmpty(l)) {\
\        print(\", \");\
\        print(l.hd);\
\        l = l.tl;\
\    }\
\    print(']');\
\}"
