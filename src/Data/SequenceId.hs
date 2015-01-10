module Data.SequenceId
       ( checkSeqId
       , nextSeqId

         -- * Monadic
       , checkSeqIdM
       , nextSeqIdM
       , SequenceT
       , evalSequenceT

         -- * Types
       , SequenceError (..)
       , SequenceIds (..)
       , SequenceId
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


------------------------------------------------------------------------------
-- | If the current sequence ID is greater than 1 more than the last sequence ID then the appropriate error is returned.
checkSeqIdM :: Monad m => SequenceId -- ^ Current sequence ID
            -> (SequenceT m) (Maybe SequenceError)
checkSeqIdM currSeq = do
    lastSeq <- gets unLastSeqId
    put $ LastSeqId currSeq
    return $ checkSeqId lastSeq currSeq


------------------------------------------------------------------------------
-- | If the difference between the sequence IDs is not 1 then the appropriate error is returned.
checkSeqId :: SequenceId -- ^ Last sequence ID
           -> SequenceId -- ^ Current sequence ID
           -> Maybe SequenceError
checkSeqId lastSeq currSeq
    | (currSeq - lastSeq) > 1 = Just . SequenceIdDropped    $ SequenceIds lastSeq currSeq
    | (currSeq - lastSeq) < 1 = Just . SequenceIdDuplicated $ SequenceIds lastSeq currSeq
    | otherwise = Nothing


------------------------------------------------------------------------------
-- | Update to the next sequense ID
nextSeqIdM :: Monad m => SequenceT m SequenceId -- ^ Next sequence ID
nextSeqIdM = modify (LastSeqId . nextSeqId . unLastSeqId) >> gets unLastSeqId


------------------------------------------------------------------------------
-- | Update to the next sequense ID
nextSeqId :: SequenceId -- ^ Last sequence ID
          -> SequenceId -- ^ Next sequence ID
nextSeqId = (+1)
