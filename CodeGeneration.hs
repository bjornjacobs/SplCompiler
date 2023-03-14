{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CodeGeneration where

import AST
import Prettyprinter
import Lexer
import qualified Data.Map as Map hiding (foldl)

import Data.Char (ord)
import Data.Bits
import CompilerPhase

type Instruction = String
data Address 
    = Global Int
    | Local Int

data Environment = Environment {envm :: Map.Map String Address, labelPrefix :: String, currentFunction :: String}

empty :: Environment
empty = Environment Map.empty "" ""

insert :: String -> Address -> Environment -> Environment
insert k v (Environment env str f) = Environment (Map.insert k v env) str f

(!) :: Environment -> String -> Address
(!) = (Map.!) . envm

allocateGlobals :: Program (Position, Type) -> (Environment, [Instruction])
allocateGlobals = createInstructions . foldl allocate (empty, [], 0)
    where allocate (env, instr, counter) Function {} = (env, instr, counter)
          allocate (env, instr, counter) (Variable _ (name, _) _ _) = 
              (insert name (Global counter) env, 
              instr ++ ["annote SP " ++ show counter ++ " " ++ show counter ++ " cyan \"Global variable " ++ name ++ "\""],
              counter - 1)
          createInstructions (env, instr, counter) = (env, ("ajs " ++ show (negate counter)) : "ldr SP" : "str R5" : instr)

class Code x where
    generateCode :: Environment -> x -> [Instruction]

instance Code (Program (Position, Type)) where
    generateCode env decs = concatMap (generateCode env) decs
    
instance Code (Declaration (Position, Type)) where
    generateCode env (Variable td (name, e) expr _) = generateCode env (Assignment (Id name e) expr e)
    generateCode env (Function (name, _) args _ decls stmts _) =
        let (env', instr, _) = foldl addArg (env, [], -2) args
            addArg (env, instr, offset) (name, _) =
                (insert name (Local offset) env, 
                instr ++ ["ldl " ++ show offset] ++ refplus ++ ["ajs -1", annote "MP" offset offset "green" ("Function argument " ++ name)],
                offset - 1)
        in
        let (env'', instr', size) = foldl addLocal (env', instr, 1) decls
            addLocal (env, instr, offset) (Variable _ (name, _) expr _) = 
                (insert name (Local offset) env, 
                    instr ++ generateCode env expr ++ refplus ++
                    ["stl " ++ show offset, annote "MP" offset offset "green" ("Local variable " ++ name)],
                     offset + 1)
        in ("bra __end" ++ name) : 
           (name ++ ": link " ++ show (size - 1)) : 
           instr' ++ 
           concatMap (generateCode (env'' {labelPrefix = name ++ "_", currentFunction = name})) stmts ++ 
           ["_" ++ name ++ "_ret:"] ++
           concatMap (\(Local v) -> ("ldl " ++ show v) : refmin) (filter isLocal (map snd . Map.assocs $ envm env'')) ++
           ["unlink", "ret"] ++
           ["__end" ++ name ++ ":"]
isLocal :: Address -> Bool
isLocal (Global _) = False
isLocal (Local _) = True           

instance Code (Statement  (Position, Type)) where
    generateCode env (Assignment (Id name (_, t)) expr _) = generateCode env expr ++ case env ! name of
        Global addr -> annotation ++ ["ldr R5", "lda " ++ show addr] ++ refmin ++ ["ajs -1", "ldr R5", "sta " ++ show addr] ++ refplus
            where annotation = case t of
                    AbstractTupleType {} -> ["str R6", "ldr R6", annote "R6" 0 0 "red" ("Global " ++ name ++ ".fst"), annote "R6" 1 1  "red" ("Global " ++ name ++ ".snd")  ]
                    _ -> []
        Local addr -> ["ldl " ++ show addr, "str R6"] ++ refplus ++ ["stl " ++ show addr] ++ ["ldr R6"] ++ refmin ++ ["ajs -1"]
    generateCode env (Assignment (UnOp (op,_) expr1 e) expr2 _) = 
        generateCode env expr2 ++ refplus ++ generateCode env expr1 ++ ["ldc " ++ pm, "sub", "lds 0", "lda " ++ show offset] ++ refmin ++ ["ajs -1", "sta " ++ show offset]
        where offset = case op of
                    ".fst" -> 0
                    ".snd" -> 1
                    ".hd" -> 0
                    ".tl" -> 1
    generateCode env (CallStmt (name, _) args e) = calcArgs ++ case name of
        "printInt" -> fixSign ++ ["trap 0"]
        "printChar" -> ["trap 1"]
        "isEmpty" -> generateCode env (head args) ++ ["ldc 0", "eq", "str RR"]
        _ -> ["bsr " ++ name, "ajs " ++ show (negate $ length args)]
        where calcArgs = concatMap (generateCode env) (reverse args)
    generateCode env (Return Nothing _) = ["bra _" ++ currentFunction env ++ "_ret"]
    generateCode env (Return (Just expr) _) = generateCode env expr ++ ["str RR", "bra _" ++ currentFunction env ++ "_ret"]
    generateCode env (If c thens elses (pos, _)) = generateCode env c ++ 
        ["brf " ++ lblf] 
        ++ concatMap (generateCode (env {labelPrefix = lblt ++ "_"})) thens 
        ++ ["bra " ++ lble, lblf ++ ":"]
        ++ concatMap (generateCode (env {labelPrefix = lblf ++ "_"})) elses 
        ++ [lble ++ ":"]
            where lbl = labelPrefix env ++ show (fst pos) ++ "_" ++ show (snd pos)
                  lblf = lbl ++ "iff"
                  lble = lbl ++ "ife"
                  lblt = lbl ++ "ift"
    generateCode env (While condition stmts (pos, _)) = [lbls ++ ":"] ++ generateCode env condition ++ 
        ["brf " ++ lble] 
        ++ concatMap (generateCode (env {labelPrefix = lbl ++ "_"})) stmts 
        ++ ["bra " ++ lbls]
        ++ [lble ++ ":"]
            where lbl = labelPrefix env ++ show (fst pos) ++ "_" ++ show (snd pos)
                  lbls = lbl ++ "whilestart"
                  lble = lbl ++ "whileend"
    -- generateCode _ _ = []

refplus :: [Instruction]
refplus = ["bsr _refplus"]

refmin :: [Instruction]
refmin = ["bsr _refmin"]

refplusFunction :: [Instruction]
refplusFunction = [
    "_refplus:",
    "link 0",
    -- check if pointer
    "ldl -2",
    "ldc 0",
    "lt",
    "brf __endrefplus",
    -- subtract one from refcount if pointer
    "ldl -2",
    "ldc " ++ pm,
    "sub",
    "lda -1",
    "ldc 1",
    "add",
    -- store updated refcount if not 0
    "ldl -2",
    "ldc " ++ pm,
    "sub",
    "sta -1",

    "__endrefplus:",
    "unlink",
    "ret"   
    ]  

refminFunction :: [Instruction]
refminFunction = [
    "_refmin:",
    "link 0",
    -- check if pointer
    "ldl -2",
    "ldc 0",
    "lt",
    "brf __endrefmin",
    -- subtract one from refcount if pointer
    "ldl -2",
    "ldc " ++ pm,
    "sub",
    "lda -1",
    "ldc 1",
    "sub",
    -- check if it was the last reference
    "lds 0",
    "ldc 0",
    "eq",
    "brf _afterfree",
    -- free block if refcount is now 0
    "ajs -1",
    "ldl -2",
    "ldc " ++ pm,
    "sub",
    "bsr _free",
    "bra __endrefmin",
    "_afterfree:",
    -- store updated refcount if not 0
    "ldl -2",
    "ldc " ++ pm,
    "sub",
    "sta -1",

    "__endrefmin:",
    "unlink",
    "ret"
    ] --TODO: Check if value is a pointer and if this the case decrease the ref count


    
-- returns the next free place on heap and modifies R7 to point at the next free place
malloc :: [Instruction]
malloc = [
    "_malloc:",
    "ldr R7",
    -- "ldc " ++ pm,
    -- "add", 
    "str RR",  --Save return value
   -- "ldc" ++ pm,
   -- "sub", 
    "ldc 0",
    "ldr R7",
    "sta -1",
    annote "R7" (-1) (-1) "black" "refcount",
    annote "R7" 0 0 "red" "value1",
    annote "R7" 1 1 "red" "value2",
    "ldr R7",
    "lda 0",
    "lds 0",
    "ldc 0",
    "eq 0", -- Check if the free list has a next element
    "brf _endmalloc",  
    -- Expand heap if necessary
    "ajs -1",
    "ldc 0", -- refcount
    "ldc 0", -- next free space
    "ldc 0", -- -
    "stmh 3",
    annote "HP" (-3) (-3) "black" "free block",
    annote "HP" (-2) (-2) "red" "next free block",
    annote "HP" (-1) (-1) "black" "-",
    "ldc 1",
    "sub",
    "_endmalloc:", -- next value of R7 is at top of the stack
    "str R7",
    "ret"
    ]

-- 
free :: [Instruction]
free = [
    "_free:", -- address to be freed is at top of the stack
    "link 0",
    "ldl -2", -- copy argument
    "ldma 0 2",
    "bsr _refmin",
    "ajs -1",
    "bsr _refmin",
    "ajs -1",
    "ldc 0", -- ensure refcount is 0
    "ldr R7",
    "ldl -2",
    "stma -1 2",
    "ldl -2",
    "str R7",
    annote "R7" (-1) (-1) "black" "free block",
    annote "R7" 0 0  "red" "Next free block",
    annote "R7" 1 1  "red" "-",
    "unlink",
    "ret",
    "__end_free:"
    ]

aborthead :: [Instruction]
aborthead = [
    "__aborthead:",
    "ldc " ++ show (ord '\n'),
    "trap 1",
    "ldc " ++ show (ord '.'),
    "trap 1",
    "ldc " ++ show (ord 'h'),
    "trap 1",
    "ldc " ++ show (ord 'd'),
    "trap 1",
    "ldc " ++ show (ord ' '),
    "trap 1",
    "ldc " ++ show (ord 'o'),
    "trap 1",
    "ldc " ++ show (ord 'n'),
    "trap 1",
    "ldc " ++ show (ord ' '),
    "trap 1",
    "ldc " ++ show (ord '['),
    "trap 1",
    "ldc " ++ show (ord ']'),
    "trap 1",
    "ldc " ++ show (ord '\n'),
    "trap 1",
    "ldc -1",
    "ldc -1",
    "sta 0; generate error to avoid 'machine halted'",
    "halt"
    ]
aborttail :: [Instruction]
aborttail = [
    "__aborttail:",
    "ldc " ++ show (ord '\n'),
    "trap 1",
    "ldc " ++ show (ord '.'),
    "trap 1",
    "ldc " ++ show (ord 't'),
    "trap 1",
    "ldc " ++ show (ord 'l'),
    "trap 1",
    "ldc " ++ show (ord ' '),
    "trap 1",
    "ldc " ++ show (ord 'o'),
    "trap 1",
    "ldc " ++ show (ord 'n'),
    "trap 1",
    "ldc " ++ show (ord ' '),
    "trap 1",
    "ldc " ++ show (ord '['),
    "trap 1",
    "ldc " ++ show (ord ']'),
    "trap 1",
    "ldc " ++ show (ord '\n'),
    "trap 1",
    "ldc -1",
    "ldc -1",
    "sta 0; generate error to avoid 'machine halted'",
    "halt"
    ]

storeHeap :: (String, String) -> (String, String) -> [Instruction]
storeHeap (color1, descr1) (color2, descr2) = ["bsr _malloc", "ldr RR", "stma 0 2"]
    ++ [annote "RR" 0 0 color1 descr1, annote "RR" 1 1 color2 descr2]
    ++ ["ldr RR", annote "SP" 0 0 "red" "Allocated on heap"]
    ++ ["ldc " ++ pm, "add"]

fixSign :: [Instruction]
fixSign = ["ldc 2", "mul", "ldc 2", "div"] 

instance Code (Expr (Position, Type)) where 
    generateCode env (IntConstant v _) = ["ldc " ++ show (v .&. 0x7FFFFFFF)]
    generateCode env (CharConstant v _) = ["ldc " ++ show (ord v)]
    generateCode env (BoolConstant v _) = ["ldc " ++ if v then pm else "0"]
    generateCode env (EmptyList _) = ["ldc 0"]
    generateCode env (BinOp expr1 (":", _) expr2 _) = 
        generateCode env expr1 ++ generateCode env expr2 ++ refplus
        ++ storeHeap ("black", "list value") ("red", "list pointer")
    generateCode env (BinOp expr1 (o, _) expr2 _) = generateCode env expr1 ++ fixSign
        ++ generateCode env expr2 ++ fixSign
        ++ [getOperation o, "ldc " ++ pm, "and", annote "SP" 0 0 "red" ("Result of " ++ getOperation o)]
        where   getOperation "+" = "add"
                getOperation "-" = "sub"
                getOperation "*" = "mul"
                getOperation "/" = "div"
                getOperation "%" = "mod"
                getOperation "==" = "eq"
                getOperation ">" = "gt"
                getOperation ">=" = "ge"
                getOperation "<" = "lt"
                getOperation "<=" = "le"
                getOperation "!=" = "ne"
                getOperation "&&" = "and"
                getOperation "||" = "or"
                getOperation _ = ""
    generateCode env (UnOp ("-", _) expr _) = generateCode env expr ++ fixSign ++ ["neg", "ldc " ++ pm, "and", annote "SP" 0 0 "red" "Result of -x"]
    generateCode env (UnOp (".fst", _) expr _) = generateCode env expr ++ ["ldc " ++ pm, "sub","ldh 0", annote "SP" 0 0 "black" ".fst value"]
    generateCode env (UnOp (".snd", _) expr _) = generateCode env expr ++ ["ldc " ++ pm, "sub","ldh 1", annote "SP" 0 0 "black" ".snd value"]
    generateCode env (UnOp (".hd", _) expr _) = generateCode env expr 
            ++ ["lds 0", "ldc 0", "eq", "brt __aborthead"]
            ++ ["ldc " ++ pm, "sub", "ldh 0", annote "SP" 0 0 "black" ".hd value"]
    generateCode env (UnOp (".tl", _) expr _) = generateCode env expr 
            ++ ["lds 0", "ldc 0", "eq", "brt __aborttail"]
            ++ ["ldc " ++ pm, "sub", "ldh 1", annote "SP" 0 0 "black" ".tl value"]
    generateCode env (UnOp ("!", _) expr _) = generateCode env expr ++ ["ldc 0", "eq", "ldc " ++ pm, "and", annote "SP" 0 0 "black" ".tl value"]
    generateCode env (Tuple expr1 expr2 _) = 
        generateCode env expr1 ++ refplus ++ generateCode env expr2 ++ refplus
        ++ storeHeap ("black", "Tuple.fst") ("black", "Tuple.snd")
    generateCode env (Id name _) = case env ! name of
        Global addr -> ["ldr R5", "lda " ++ show addr]
        Local addr -> ["ldl " ++ show addr]
    generateCode env (CallExpr n ex e) = generateCode env (CallStmt n ex e) ++ ["ldr RR"]
    generateCode env _ = []

annote :: String -> Int -> Int -> String -> String -> String
annote rp lof hof color msg = "annote " ++ rp ++ " " ++ show lof ++ " " ++ show hof ++ " " ++ color ++ " \"" ++ msg ++ "\"" 

codeGenerator :: CompilerPhase (Program (Position, Type)) [Instruction]
codeGenerator ast = 
    let (env, start) = allocateGlobals ast
        initheap = [
            "ldc 0",
            "ldc 0",
            "ldc 0",
            "stmh 3",
            annote "HP" (-3) (-3) "black" "free block",
            annote "HP" (-2) (-2) "red" "next free block",
            annote "HP" (-1) (-1) "black" "-",
            "ldc 1",
            "sub",
            "str R7"
            ]
    in return $ initheap 
        ++ start 
        ++ ["bra __end_free"] 
        ++ refplusFunction ++ refminFunction ++ aborthead ++ aborttail ++ malloc ++ free 
        ++ generateCode env ast ++ ["bsr main"]

    
--Value used to denote a pointer    
pm = "2147483647" -- 0x7FFFFFFF