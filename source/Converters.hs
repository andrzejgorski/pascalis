module Converters where

import Data.Array
import Data.Char

import AbsPascalis
import PrintPascalis
import MonadsFunctions
import ErrM


printDebug [] = return ()
printDebug (h:t) = do{putStr_IO h; printDebug t}

-- Type functions
getType :: EExp -> MRSIO Type
getType exp = case exp of
    BTrue       -> return TBool
    BFalse      -> return TBool
    BNot _      -> return TBool

    EOr _ _     -> return TBool
    EAnd _ _    -> return TBool
    EAss _ _    -> return TBool
    ENAss _ _   -> return TBool
    ELt _ _     -> return TBool
    EGt _ _     -> return TBool
    ELEt _ _    -> return TBool
    EGEt _ _    -> return TBool

    EAdd e _    -> getType e
    ESub _ _    -> return TInt
    EMul _ _    -> return TInt
    EDiv _ _    -> return TInt

    EInt _      -> return TInt
    EChar _     -> return TChar
    EStr _      -> return TStr
    EFSub _     -> return TStr
    ELSub _ _   -> return TStr
    ERSub _ _   -> return TStr
    ELRSub _ _ _-> return TStr
    -- TODO consider it
    EKey e _    -> do con_t <- getType e
                      return $ getContainerValueType con_t
    ELen _      -> return TFunc
    EOrd _      -> return TFunc
    EArrII _    -> return (TArr TInt TInt)
    EVar v      -> askType v


getContainerKeyType :: Type -> Type
getContainerKeyType TStr     = TInt
getContainerKeyType (TArr t1 t2)  = t1


getContainerValueType :: Type -> Type
getContainerValueType TStr   = TChar
getContainerValueType (TArr t1 t2)= t2


-- converters utils functions
convertVar converter ident = do v <- askExp ident
                                converter v


intFromEInt :: EExp -> MRSIO Int
intFromEInt i = do int <- calcInt i
                   return $ fromIntegral $ int


concatenation (EStr s1) (EStr s2) = EStr (s1 ++ s2)


-- Converter functions
getConverter :: Type -> (EExp -> MRSIO EExp)
getConverter t = case t of
    TBool      -> calcBool
    TInt       -> calcExpInt
    TChar      -> calcChar
    TStr       -> calcString
    TFunc      -> calcFunc
    TArr _ _   -> calcArr


calcBool :: EExp -> MRSIO EExp
calcBool exp = case exp of
    -- bool expressions
    BTrue           -> return BTrue
    BFalse          -> return BFalse
    BNot v          -> do bexp <- calcBool v
                          if bexp == BTrue then
                            return BFalse
                          else
                            return BTrue
    EAnd exp1 exp2  -> do bexp <- calcBool exp1
                          if bexp == BTrue then
                            calcBool exp2
                          else
                            return BFalse
    EOr exp1 exp2   -> do bexp <- calcBool exp1
                          if bexp == BTrue then
                            return BTrue
                          else
                            calcBool exp2
    EAss exp1 exp2  -> calcConvered (==) exp1 exp2
    ENAss exp1 exp2 -> calcConvered (/=) exp1 exp2
    ELt exp1 exp2   -> calcConvered (<)  exp1 exp2
    EGt exp1 exp2   -> calcConvered (>)  exp1 exp2
    ELEt exp1 exp2  -> calcConvered (<=) exp1 exp2
    EGEt exp1 exp2  -> calcConvered (>=) exp1 exp2
    EVar i          -> convertVar calcBool i
  where
    calcConvered func exp1 exp2 = do {
        t1 <- getType exp1;
        t2 <- getType exp2;
        if t1 == t2 then
          let converter = getConverter t1;
          in do con1 <- converter exp1
                con2 <- converter exp2
                if func con1 con2 then
                  return BTrue
                else
                  return BFalse
        else
            -- TODO handle errors
          return BFalse
      }


calcInt :: EExp -> MRSIO Integer
calcInt x = case x of
    EVar ident     -> convertVar calcInt ident
    EAdd exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 + n2
    ESub exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 - n2
    EMul exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 * n2
    EDiv exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 `div` n2
    EInt n         -> return n
    EKey cont k    -> getFromCont cont k
      where
        getFromCont (EArrII a) (EInt i1) = do i <- intFromEInt (EInt i1)
                                              return $ toInteger $ a ! i

        getFromCont (EVar ident) key     = do a <- askExp ident
                                              getFromCont a key


calcExpInt :: EExp -> MRSIO EExp
calcExpInt exp = do int <- calcInt exp
                    return $ EInt $ toInteger int


calcChar :: EExp -> MRSIO EExp
calcChar (EChar c) = return $ EChar c
calcChar (EVar i)  = convertVar calcChar i
calcChar (EKey container k) = getFromCont container k
  where
    getFromCont (EStr s) k       = do i <- intFromEInt k
                                      return $ EChar $ head $ drop i s
    getFromCont (EVar ident) key = do a <- askExp ident
                                      getFromCont a key

calcString :: EExp -> MRSIO EExp
calcString str = case str of
    EStr s              -> return $ EStr s
    EAdd s1 s2          -> do
                              str1 <- calcString s1
                              str2 <- calcString s2
                              return $ concatenation str1 str2

    EFSub s             -> return s

    ELSub (EStr s) l    -> do i <- intFromEInt l
                              return $ EStr $ drop i s
    ERSub (EStr s) l    -> do i <- intFromEInt l
                              return $ EStr $ take i s
    ELRSub (EStr s) l r -> do {
        left <- intFromEInt l;
        right <- intFromEInt r;
        return $ EStr ((take (right - left)) $ drop left s)
      }
    EVar i              -> convertVar calcString i



calcFunc :: EExp -> MRSIO EExp
calcFunc (ELen (EStr s)) = return $ EInt (toInteger $ length s)
calcFunc (EOrd (EChar c)) = return $ EInt (toInteger $ ord c)


calcArr :: EExp -> MRSIO EExp
calcArr = (\x -> return x)


calcExp :: EExp -> MRSIO EExp
calcExp e = do t <- getType e
               getConverter t e

rangeExp :: EExp -> EExp -> Int
rangeExp (EInt i1) (EInt i2) = fromInteger $ i2 - i1 + 1

nextExp :: EExp -> MRSIO EExp
nextExp (EInt i) = return (EInt (i + 1))
