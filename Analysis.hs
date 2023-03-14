{-# LANGUAGE FlexibleInstances #-}
module Analysis where
import Data.Graph
import Data.List hiding (partition)
import AST
import Parser
import ParserCombinators
import Lexer
import Control.Applicative
import Data.Tree
import Data.Maybe
import CompilerPhase
import Util

partition :: Program e -> [[Declaration e]]
partition program = let (g, f, m) = toCallGraph program
                        sccs = map flatten $ scc $ transposeG g in
                            reverse $ map (map (fst3 . f)) sccs
                            
validComponents :: CompilerPhase [[Declaration Position]] [[Declaration Position]]
validComponents = collectErrors . map validComponent
    where validComponent v@[Variable {}] = Right v
          validComponent c = mapM isFunc c
          isFunc f@Function {} = Right f
          isFunc (Variable _ (name, e) _ _) = Left $ CycleError ("Variable '" ++ name ++ "' is contained in a cyclic dependency") e

tarjan :: CompilerPhase (Program Position) [[Declaration Position]]
tarjan = validComponents . partition
                        
toCallGraph :: Program e -> (Graph, Vertex -> (Declaration e, String, [String]), String -> Maybe Vertex)
toCallGraph = graphFromEdges . map toNode

toNode :: Declaration e -> (Declaration e, String, [String])
toNode decl@(Variable _ (name, _) expr _) = (decl, name, dependencies expr [])
toNode decl@(Function (name, _) args _ decls stmts _) = (decl, name, 
    dependencies stmts locals `union` nub (concatMap dep decls))
        where locals = map getName decls ++ map fst args
              dep decl@(Variable _ _ expr _) = dependencies expr  locals
              dep _ = []

getName :: Declaration e -> String
getName (Variable _ (name, _) _ _) = name
getName (Function (name, _) _ _ _ _ _) = name

class Dependencies x where
    dependencies :: x -> [String] -> [String]

instance Dependencies (Expr e) where
    dependencies (Id name _) locals = [name | name `notElem` locals]
    dependencies (BinOp e1 _ e2 _) locals = dependencies e1 locals `union` dependencies e2 locals
    dependencies (UnOp _ e _) locals = dependencies e locals
    dependencies (CallExpr (name, _) exprs _) locals = foldl (\a b -> a `union` dependencies b locals) [name | name `notElem` locals] exprs
    dependencies (Tuple e1 e2 _) locals = dependencies e1 locals `union` dependencies e2 locals
    dependencies _ _ = []
    
instance Dependencies [Statement e] where
    dependencies stmts locals =  nub $ concatMap dep stmts
          where dep (If expr thens elses _) = dependencies expr locals `union` dependencies (thens ++ elses) locals
                dep (While expr stmts _) = dependencies expr locals `union` dependencies stmts locals
                dep (Assignment expr1 expr2 _) = dependencies expr1 locals `union` dependencies expr2 locals
                dep (CallStmt (name, _) exprs _) = foldl (\a b -> a `union` dependencies b locals) [name | name `notElem` locals] exprs
                dep (Return Nothing _) = []
                dep (Return (Just expr) _) = dependencies expr locals

-- graphFromEdges :: Ord key => [(node, key, [key])] -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
-- fromCallGraph :: [[Vertex]] -> [[Declaration e]]

fst3 (a, _, _) = a

-- ttest :: [Statement Position]
-- ttest = fst $ head $ fst3 $ parsef (many pStmt) "x = 7; if (x) {x = 6; y = 5; f(z);} else {while (y) { y = y - 1;}} y = 8; return 5;"

returnPathAnalysis :: Declaration e -> Either (String, e) (Declaration e)
returnPathAnalysis v@Variable {} = Right v
returnPathAnalysis ((Function (name, nameE) ns td decs stmts e)) = do
    let rc = returnCount stmts
    let stmts' = if rc == 0 then stmts ++ [Return Nothing e] else stmts
    if rpa stmts' then
        Right $ Function (name, nameE) ns td decs stmts' e
    else
        Left ("Not all code paths have a return value, function: " ++ name, e)

pathAnalysis :: CompilerPhase (Program Position) (Program Position)
pathAnalysis = combineErrors (mapLeft ((:[]) . uncurry ReturnPathError) . returnPathAnalysis)


rpa :: [Statement e] -> Bool
rpa = any g -- && z (map g stmts)
    where g (If _ thens elses _) = rpa thens && rpa elses
          g (While _ stmts _) = False
          g (Return _ _) = True
          g _  = False
          z [_] = True
          z (True:xs) = False
          z (False:xs) = z xs
          


returnCount :: [Statement e] -> Int
returnCount = sum . map g
    where g (If _ thens elses _) = returnCount thens + returnCount elses
          g (While _ stmts _) =  returnCount stmts 
          g (Return _ _) = 1
          g _  = 0