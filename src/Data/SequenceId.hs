module Data.SequenceId
       ( checkSeqIdM
       , checkSeqId
       , nextSeqIdM
       , nextSeqId
       , SequenceError (..)
       , SequenceIds (..)
       , SequenceId
       , SequenceT
       , evalSequenceT
       ) where


import           Control.Monad.Trans.State (StateT, evalStateT, gets, modify,
                                            put)
import           Data.Word                 (Word32)


newtype LastSeqId = LastSeqId { unLastSeqId :: SequenceId } deriving Show
type SequenceT = StateT LastSeqId
type SequenceId = Word32


evalSequenceT :: Monad m => SequenceId -> StateT LastSeqId m b -> m b
evalSequenceT = flip evalStateT . LastSeqId


data SequenceIds =
    SequenceIds
    { lastSeqId :: SequenceId
    , currSeqId :: SequenceId
    } deriving (Eq, Show)


data SequenceError
    = SequenceIdDropped SequenceIds
    | SequenceIdDuplicated SequenceIds
    deriving (Eq, Show)


checkSeqIdM :: Monad m => SequenceId -> (SequenceT m) (Maybe SequenceError)
checkSeqIdM currSeq = do
    lastSeq <- gets unLastSeqId
    put $ LastSeqId currSeq
    return $ checkSeqId lastSeq currSeq


checkSeqId :: SequenceId -> SequenceId -> Maybe SequenceError
checkSeqId lastSeq currSeq
    | (currSeq - lastSeq) > 1 = Just . SequenceIdDropped    $ SequenceIds lastSeq currSeq
    | (currSeq - lastSeq) < 1 = Just . SequenceIdDuplicated $ SequenceIds lastSeq currSeq
    | otherwise = Nothing


nextSeqIdM :: Monad m => SequenceT m SequenceId
nextSeqIdM = modify (LastSeqId . nextSeqId . unLastSeqId) >> gets unLastSeqId


nextSeqId :: SequenceId -> SequenceId
nextSeqId = (+1)
