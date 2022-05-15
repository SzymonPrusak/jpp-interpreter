module TypeHelper where

import Gram.Abs



intTn :: TypeName
intTn = TNPrim Nothing $ PTInt Nothing
stringTn :: TypeName
stringTn = TNPrim Nothing $ PTString Nothing
boolTn :: TypeName
boolTn = TNPrim Nothing $ PTBool Nothing

arrayTn :: TypeName -> TypeName
arrayTn tn = TNArr Nothing $ TArrayType Nothing tn

tupleTn :: [TypeName] -> TypeName
tupleTn tn = TNTuple Nothing $ TTupleType Nothing $ map (TupleSType Nothing) tn

readWriteTd :: TypeName -> TypeDef
readWriteTd tn = TypeDefin Nothing tn $ TMNone Nothing
readOnlyTd :: TypeName -> TypeDef
readOnlyTd tn = TypeDefin Nothing tn $ TMReadonly Nothing


compareTypes :: TypeName -> TypeName -> Bool
compareTypes (TNPrim _ t1) (TNPrim _ t2) = comparePrimTypes t1 t2 where
    comparePrimTypes :: PrimType -> PrimType -> Bool
    comparePrimTypes PTBool {} PTBool {} = True
    comparePrimTypes PTString {} PTString {} = True
    comparePrimTypes PTInt {} PTInt {} = True
    comparePrimTypes _ _ = False
compareTypes (TNArr _ (TArrayType _ t1)) (TNArr _ (TArrayType _ t2)) = compareTypes t1 t2
compareTypes (TNTuple _ (TTupleType _ st1)) (TNTuple _ (TTupleType _ st2)) = compareTupleSubTypes st1 st2 where
    compareTupleSubTypes :: [TupleSubType] -> [TupleSubType] -> Bool
    compareTupleSubTypes l1 l2
        | length l1 /= length l2 = False
        | otherwise = all doComparison $ zip l1 l2 where
            doComparison (TupleSType _ t1, TupleSType _ t2) = compareTypes t1 t2
compareTypes _ _ = False
