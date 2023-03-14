{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Typing where

import CompilerPhase
import AST
import Data.Functor
import Data.List
import Data.Maybe

import Control.Monad.Except
import Control.Monad.State

import Util
import Prettyprinter
import Data.Bifunctor
import Data.Char

import qualified Debug.Trace

debug :: Bool
debug = False

trace :: String -> a -> a
trace s x = if debug then Debug.Trace.trace s x else x

isFunc :: Declaration e -> Bool
isFunc Function {} = True
isFunc _ = False

data Scheme = Scheme [String] Type

instance Show Scheme where
    show (Scheme bound t) = "forall " ++ intercalate ", " bound ++ ". " ++ show t

getType (Scheme _ t) = t
type Substitution = [(String, Type)]
type Environment = [(String, Scheme)]

class Substitutable x where
    apply :: Substitution -> x -> x

instance {-# OVERLAPPABLE #-} Substitutable x where
    apply = const id

instance Substitutable Type where
    apply sub (AbstractTupleType a b) = AbstractTupleType (apply sub a) (apply sub b)
    apply sub (AbstractListType a) = AbstractListType (apply sub a)
    apply sub (FunctionType args t) = FunctionType (map (apply sub) args) (apply sub t)
    apply ((i, s):ts) (TypeVariable j)
        | j == i = s
        | otherwise = apply ts (TypeVariable j)
    apply sub x = x

instance Substitutable Environment where
    apply sub = map apply'
        where apply' (str, Scheme xs t) = (str, Scheme xs (apply sub t))

infixl 4 <.>
(<.>) :: Substitution -> Substitution -> Substitution

a <.> b = map (\x -> (x, apply a $ apply b $ TypeVariable x)) xs
    where xs = map fst a `union` map fst b

class Composable x where
    compose :: x -> Substitution -> x

instance Composable Substitution where
    compose m u = m <.> u

instance {-# OVERLAPPABLE #-} (Monad a, Composable c) => Composable (a c) where
    compose m u = m <&> flip compose u

instance (Composable s) => Composable (s, x) where
    compose (s, x) u = (compose s u, x)

instance (Composable a, Substitutable s) => Composable (s -> a) where
    compose m v x = compose (m (apply v x)) v

data ErrorReason
    = CouldNotUnify
    | InfiniteType
    deriving (Show, Enum)

data TypeError e = TypeError {expected :: Type, actual :: Type, variables :: [(String, Type)], reason :: ErrorReason, inExpr :: Expr e}

instance Show (TypeError e) where
    show t = show' t ++ " Relevant variables include: " ++ relevantVars t ++ " in expression \"" ++ prettyprint (inExpr t) ++ "\""
        where show' (TypeError e a vars CouldNotUnify _) = "Expected type \"" ++ show e ++ "\", but got \"" ++ show a ++ "\"."
              show' (TypeError e a vars InfiniteType _) = "Cannot construct infinity type \"" ++ show e ++ " ~ " ++ show a ++ "\"."
              relevantVars (TypeError e a vars _ _) = show vars

addEnv :: Environment -> TypeError e -> TypeError e
addEnv env (TypeError a b c d e) = TypeError a b (c ++ map (second getType) env) d e
addExpr :: Expr e -> TypeError d -> TypeError e
addExpr e (TypeError a b c d _) = TypeError a b c d e

type ErrorResult e = Either (TypeError e, e)
type StateErrorResult e = StateT Int (Either (String, e))

instance MonadFail (Either String) where
    fail = Left

instance MonadFail (Either (String, e)) where
    fail = undefined

freeVariables :: Type -> [String]
freeVariables (TypeVariable i) = [i]
freeVariables (AbstractTupleType a b) = freeVariables a `union` freeVariables b
freeVariables (AbstractListType a) = freeVariables a
freeVariables (FunctionType args ret) = concatMap freeVariables args `union` freeVariables ret
freeVariables _ = []

freeVars :: Scheme -> [String]
freeVars (Scheme bound t) = [var | var <- freeVariables t, var `notElem` bound]

unification :: Type -> Type -> ErrorResult () Substitution
unification s t = mapLeft (\(TypeError _ _ v r e, ()) -> (TypeError t s v r e, ())) $ impl s t
    where impl IntType IntType = Right []
          impl BoolType BoolType = Right []
          impl CharType CharType = Right []
          impl VoidType VoidType = Right []
          impl (TypeVariable i) (TypeVariable j)
            | isDigit (head j) && not (isDigit (head i)) = impl (TypeVariable j) (TypeVariable i)
            | i == j = Right []
            | otherwise = Right [(i, TypeVariable j)]
          impl (TypeVariable i) t
            | i `elem` freeVariables t = Left (TypeError (TypeVariable i) t [] InfiniteType (EmptyList ()), ())
            | otherwise = Right [(i, t)]
          impl t (TypeVariable i) = unification (TypeVariable i) t
          impl (AbstractListType a) (AbstractListType b) = unification a b
          impl (AbstractTupleType a b) (AbstractTupleType c d) = do
            u <- unification a c
            compose unification u b d
          impl (FunctionType [] a) (FunctionType [] b) = unification a b
          impl (FunctionType (a:as) ra) (FunctionType (b:bs) rb) = do
            u <- unification a b
            compose unification u (FunctionType as ra) (FunctionType bs rb)
          impl x y = Left (TypeError y x [] CouldNotUnify (EmptyList ()), ())

unification' env e a b = mapLeft (bimap (show . addEnv env) (const e)) $ unification a b
unification'' expr env e a b = mapLeft (bimap (show . addEnv env . addExpr expr) (const e)) $ unification a b

class M x where
    m :: Environment -> x e -> Type -> StateErrorResult e (Substitution, x (e, Type))

instance M Expr where
    -- m :: Environment -> Expr e -> Type -> StateErrorResult e (Substitution, Expr (e, Type))
    m env expr@(IntConstant i e) s = lift $ (, IntConstant i (e, IntType)) <$> unification'' expr env e IntType s
    m env expr@(BoolConstant b e) s = lift $ (, BoolConstant b (e, BoolType)) <$> unification'' expr env e BoolType s
    m env expr@(CharConstant c e) s = lift $ (, CharConstant c (e, CharType)) <$> unification'' expr env e CharType s
    m env expr@(EmptyList e) s = do
        a <- freshvar
        lift $ (, EmptyList (e, AbstractListType a)) <$> unification'' expr env e (AbstractListType a) s
    m env (CallExpr (name, ne) exps e) s = do
        fs <- replicateM (length exps) freshvar
        (u, _) <- m env (Id name e) (FunctionType fs s)
        let g (v', exprs) (exp, f) = do
            (u', expr) <- compose m v' env exp f
            return (u', exprs ++ [expr])
        (u', exprs) <- foldM g (u, []) (zip exps fs)
        return (u', CallExpr (name, (ne, FunctionType fs s)) exprs (e, s))

    m env expr@(Tuple a b e) s = do
        vars <- replicateM  2 freshvar
        case vars of
            [a1, a2] -> do
                (u', e1) <- m env a a1
                (u, e2) <- compose m u' env b a2
                lift $ (,Tuple e1 e2 (e, AbstractTupleType a1 a2)) <$> compose (unification'' expr env e) u (AbstractTupleType a1 a2) s

    m env expr@(Id name e) s = case lookup name env of
                Nothing -> trace (show env) $ lift $ Left ("Unknown variable " ++ name, e)
                Just (Scheme xs t) -> do
                    beta <- foldM (\a b -> (:a) . (b,) <$> freshvar) [] xs
                    lift $ (,Id name (e, s)) <$> unification'' expr env e s (apply beta t)

    m env expr@(BinOp e1 (o, l) e2 e) s
        | o `elem` ["+", "-", "/", "*", "%"] = binop IntType IntType IntType
        | o `elem` [">", ">=", "<", "<=", "!="] = binop IntType IntType BoolType
        | o `elem` ["!=", "&&", "||"] = binop BoolType BoolType BoolType
        | o == ":" = do
            f <- freshvar
            binop f (AbstractListType f)(AbstractListType f)
        | o == "==" = do
            f <- freshvar
            binop f f BoolType
        | otherwise = lift $ Left ("Invalid operator " ++ show o, l)
            where binop a b c = do
                    (u, e1') <- m env e1 a --`catchError` \(str, e) -> throwError (str ++ " in left hand side of " ++ prettyprint expr, e)
                    (u', e2') <- compose m u env e2 b --`catchError` \(str, e) -> throwError (str ++ " in right hand side " ++ prettyprint expr, e)
                    lift $ (,BinOp e1' (o, (l, FunctionType [a, b] c)) e2' (e, c)) <$> compose (unification' env e) u' c s

    m env (UnOp (o, l) ex e) s
        | o == "!" = unop BoolType BoolType
        | o `elem` ["-", "+", "++", "--"] = unop IntType IntType
        | o == ".fst" = do
            f1 <- freshvar
            f2 <- freshvar
            unop (AbstractTupleType f1 f2) f1
        | o == ".snd" = do
            f1 <- freshvar
            f2 <- freshvar
            unop (AbstractTupleType f1 f2) f2
        | o == ".hd" = do
            f <- freshvar
            unop (AbstractListType f) f
        | o == ".tl" = do
            f <- freshvar
            unop (AbstractListType f) (AbstractListType f)
        | otherwise = lift $ Left ("Invalid operator " ++ show o, l)
            where unop a b = do
                    (u, ex') <- m env ex a
                    lift $ (,UnOp (o, (l, FunctionType [a] b)) ex' (e, b)) <$> compose (unification' env e) u b s



instance M Statement where
    m env stmt t = m' env stmt t `catchError` \(str, err) -> throwError (str ++ " in statement \"" ++ init (prettyprint stmt) ++ "\"", err)
          where m' env (Return Nothing e) t = lift $ (, Return Nothing (e, VoidType)) <$> unification' env e VoidType t
                m' env (Return (Just expr) e) t = (\(u, ex) -> (u, Return (Just ex) (e, t))) <$> m env expr t
                m' env (Assignment lexpr rexpr e) t = do
                    f1 <- freshvar
                    (u, ex) <- m env lexpr f1
                    (u', ex') <- compose m u env rexpr f1
                    return (u', Assignment ex ex' (e, t))
                m' env (While expr stmts e) t = do
                    (u, expr') <- m env expr BoolType
                    let g (v, stms) stmt = do
                        (v', stmt') <- compose m v env stmt t
                        return (v', stms ++ [stmt'])
                    (u', stmts') <- foldM g (u, []) stmts
                    return (u', While expr' stmts' (e, t))
                m' env (CallStmt (name, e) args l) t = do
                    case lookup name env of
                        Nothing -> lift $ Left ("Unknown variable " ++ name, e)
                        Just (Scheme xs t') -> do
                            f <- freshvar
                            (u, CallExpr (n, e') args' l') <- m env (CallExpr (name, e) args l) f
                            return (u, CallStmt (n, e') args' (l, t))

                m' env (If c thens elses e) t = do
                    (u, c') <- m env c BoolType
                    let g (v, stms) stmt = do
                        (v', stmt') <- compose m v env stmt t
                        return (v', stms ++ [stmt'])
                    (u', thens') <- foldM g (u, []) thens
                    (u'', elses') <- foldM g (u', []) elses
                    return (u'', If c' thens' elses' (e,t))


instance M TypeDefinition  where
    m env dt t = lift $ (, giveType dt t) <$> unification' env (getExtra dt) (typeDefToType dt) t


--md :: Environment -> Declaration e -> Type ->  StateErrorResult e (Substitution, Scheme)

giveType :: TypeDefinition e -> Type -> TypeDefinition (e, Type)
giveType (SimpleType n e) t = SimpleType n (e, t)
giveType (TupleType td1 td2 e) t = TupleType (giveType td1 t)  (giveType td2 t) (e, t)
giveType (ListType td e) t = ListType (giveType td t) (e, t)

giveTypeDef :: Maybe ( [TypeDefinition e], TypeDefinition e) -> Type -> Maybe ([TypeDefinition (e, Type)], TypeDefinition (e, Type))
giveTypeDef (Just (tds, td)) t = Just (map (`giveType` t) tds, giveType td t)
giveTypeDef Nothing t = Nothing

instance M Declaration  where
    m  env (Variable td (name, ne) expr e) f = do
        let isVar (SimpleType "var" _) = True
            isVar _ = False
        u <- if isVar td then return [] else lift $ unification' env e (typeDefToType td) f
        (s, expr') <- compose m u env expr f
        lift $ pure (s, Variable (giveType td f) (name, (ne, f)) expr' (e, f))

            -- do
            -- u <- m ((name, Scheme [] f):env) expr f
            -- return (u, Scheme [] f)
    m  env (Function (name, nameE) args typeDef decls stmts e) f = trace ("Name: " ++ name ++ ", env: " ++ show env) $ do
        if argsLength typeDef /= -1 && length args /= argsLength typeDef then do
            lift $ Left ("Length of arguments is not the same as type signature", e)
        else if map fst args /= nub (map fst args) then do
            lift $ Left ("There are duplicate arguments", e)
        else do
            argsSchemes <- mapM toScheme (argTypePairs typeDef)
            ret <- case typeDef of
                Nothing -> freshvar
                Just (_, ft) -> return $ typeDefToType ft
            let env' = [(name, Scheme [] (FunctionType (map (\(_, Scheme _ t) -> t) argsSchemes) ret))] ++ argsSchemes ++ env
            (u, env'', decs') <- typeProgram env' (map (:[]) decls)
            let g (v, stms) stmt = do
                (v', stmt') <- trace ("env''" ++ show env'') $ compose m v env'' stmt ret
                trace (show v') $ return (v', stms ++ [stmt'])
            (rt, stmts') <- foldM g (u, []) stmts
            u <- lift $ unification' env e (FunctionType (map (\(_, Scheme _ t) -> apply rt t) argsSchemes) (apply rt ret)) f
            lift $ pure (u <.> rt, second (apply rt) <$> Function (name, (nameE, FunctionType (map (\(_, Scheme _ t) -> t) argsSchemes) ret)) (zipWith (\(_, Scheme _ t) (n, e) -> (n, (e, t))) argsSchemes args) (giveTypeDef typeDef f) decs' stmts' (e, f))
            where   argsLength (Just (ats, _)) = length ats
                    argsLength Nothing = -1
                    argTypePairs (Just (ats, _)) = zipWith (\a b -> (a, Just b)) args ats
                    argTypePairs Nothing = map (, Nothing) args
                    toScheme ((x, _), Just t) = do
                        return (x, Scheme [] (typeDefToType t))
                    toScheme ((x, _), Nothing) = do
                        f <- fresh
                        return (x, Scheme [] (TypeVariable f))

typeProgram :: Environment -> [[Declaration e]] -> StateErrorResult e (Substitution, Environment, [Declaration (e, Type)])
typeProgram genv decls = (\(a,b,c)->(a, b ++ genv, c)) <$> foldl g (pure ([], [], [])) decls
    where
    g :: StateErrorResult e (Substitution, Environment, [Declaration (e, Type)]) -> [Declaration e] -> StateErrorResult e (Substitution, Environment, [Declaration (e, Type)])
    g envm v = do
        (u, env, decls) <- envm
        (env', alpha) <- let
            g (a, as) b = do
                let name = getName b
                case lookup name a of
                    Just _ -> throwError ("Duplicate variable " ++ name, getExtra b)
                    Nothing -> do
                        f <- freshvar
                        return (a ++ [(name, Scheme [] f)], as ++ [f])
            in foldM g (env, []) v
        (u', v') <- let
            g (u, vs) (decl, var) = do
                (u', d') <- if isFunc decl then compose m u (genv ++ env') decl var else compose m u (genv ++ env) decl var
                return (u', vs ++ [d'])
            in foldM g (u, []) (zip v alpha)
        let env'' = apply u' env'
        let beta = zipWith (\a decl -> (getName decl, Scheme [var | isFunc decl, var <- freeVariables (apply u' a), var `notElem` concatMap (freeVars . snd) (genv ++ env)] (apply u' a))) alpha v
        let wrongFunctions = filter (\(_,a,b,_) -> a /= b) $ -- Check if type annotation of function definition matches the inferred type
                map ((\((name, Scheme _ actual), (expected, e)) -> (name, actual, expected, e)) . second (first toftype)) $
                mapMaybe (secondM (firstM typeDefs)) $ zipWith (\a b -> (a, (b, getExtra b))) beta v
        case wrongFunctions of
            [] -> return (u', beta ++ filter (\(a,_) -> a `notElem` map getName v) env'', map (fmap (second $ apply u')) (decls ++ v'))
            ((a, b, c, d):_) -> throwError ("Cannot match " ++ show b ++ " with " ++ show c ++ " in " ++ a, d)
        -- let name = getName v
        -- let e = getExtra v
        -- case lookup name env of
        --         Just _ -> throwError ("Duplicate variable " ++ name, e)
        --         Nothing -> do
        --             f <- freshvar
        --             let env' = env
        --             (u', scheme) <- (\(u', scheme) -> (u' <.> u, scheme)) <$>  md f (apply u env') v 
        --             let scheme' = case scheme of 
        --                             Scheme _ t@(FunctionType args ret) -> Scheme (freeVariables t) t
        --                             s -> scheme
        --             return (u', apply u' ((name, scheme'):env))
    getName (Variable _ (name, _) _ _) = name
    getName (Function (name, _) _ _ _ _ _) = name
    typeDefs Variable {} = Nothing
    typeDefs (Function _ _ td _ _ _) = td
    toftype :: ([TypeDefinition e], TypeDefinition e) -> Type
    toftype = uncurry FunctionType . bimap (map typeDefToType) typeDefToType

typeDefToType :: TypeDefinition e -> Type
typeDefToType (SimpleType n _)
    | n == "Int" = IntType
    | n == "Bool" = BoolType
    | n == "Char" = CharType
    | n == "Void" = VoidType
    | otherwise = TypeVariable n
typeDefToType (TupleType t1 t2 _ ) = AbstractTupleType (typeDefToType t1) (typeDefToType t2)
typeDefToType (ListType t _) = AbstractListType (typeDefToType t)

suba, subb, subc :: Substitution

suba = [("1", AbstractListType (TypeVariable "2"))]
subb = [("2", IntType), ("3", BoolType)]
subc = [("1", AbstractListType IntType), ("3", BoolType)]

typea = FunctionType [IntType] (TypeVariable "1")
typeb = FunctionType [TypeVariable "2", IntType] IntType

-- env :: Environment
-- env = [("f", Scheme ["1", "2"] (TypeVariable "1"))]
-- expr = Tuple (BoolConstant (True, ())) (IntConstant (5, ()))
-- expr = CallExpr ("f", ()) []
-- expr = BinOp (IntConstant 2 ()) ("==", ()) (IntConstant 2 ()) () --CallExpr ("f", ()) [IntConstant (5, ()), BoolConstant (True, ())]
-- -- f(5, 5) 
-- -- f :: () -> (Int -> (Bool -> a))
-- -- result = m env expr (TypeVariable "0")
-- runresult = runStateT result 2

-- stmt = CallStmt ("f", ()) [IntConstant 5 (), BoolConstant True ()]

fresh :: (Monad m) => StateT Int m String
fresh = do
    x <- get
    put (x + 1)
    return $ show x

freshvar :: (Monad m) => StateT Int m Type
freshvar = TypeVariable <$> fresh

typeInference :: CompilerPhase [[Declaration Position]] (Program (Position, Type))
typeInference parts = case evalStateT (
    typeProgram [
            ("print", Scheme ["t"] (FunctionType [TypeVariable "t"] VoidType)),
            ("isEmpty", Scheme ["t"] (FunctionType [AbstractListType(TypeVariable "t")] BoolType))
        ]
        parts
    ) 0 of
        Left (str, pos) -> Left [TypingError str pos]
        Right (_, _, result) -> Right result

typeChecker :: CompilerPhase [[Declaration Position]] (Program (Position, Type))
typeChecker x = typeInference x >>= checkInvalidTypes

checkInvalidTypes :: CompilerPhase (Program (Position, Type)) (Program (Position, Type))
checkInvalidTypes x = mapM_ impl x >> return x
    where
    impl (Variable _ _ _ (pos, t)) = assignable pos t
    impl (Function _ _ _ decls stmts (pos, t))
        | isHigherOrder t = Left [TypingError "Higher order functions are not supported" pos]
        | otherwise = mapM_ impl decls >> mapM_ impl' stmts

    impl' (Assignment expr _ _) = uncurry assignable (getExtra expr)
    impl' _ = return ()

    assignable pos t = case t of
        VoidType -> Left [TypingError "Variables can't be of type Void" pos]
        FunctionType {} -> Left [TypingError "Function variables are not supported" pos]
        _ -> return ()
    isHigherOrder :: Type -> Bool
    isHigherOrder (FunctionType fs _) = any isFunction fs
        where isFunction FunctionType{} = True
              isFunction _ = False
    isHigherOrder _ = False

