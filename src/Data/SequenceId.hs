module Data.SequenceId
       ( checkSeqId
       , nextSeqId

         -- * Monadic
       , checkSeqIdM
       , nextSeqIdM
       , SequenceIdT
       , evalSequenceIdT

         -- * Types
       , SequenceIdError (..)
       , SequenceIdErrorType (..)
       , SequenceId
       ) where


import           Control.Monad.Trans.State (StateT, evalStateT, get, modify',
                                            put)
import           Data.Word                 (Word32)


type SequenceIdT  = StateT SequenceId
type SequenceId   = Word32


evalSequenceIdT :: Monad m => SequenceId -> SequenceIdT m b -> m b
evalSequenceIdT = flip evalStateT


data SequenceIdError =
    SequenceIdError
    { errType   :: !SequenceIdErrorType
    , lastSeqId :: !SequenceId
    , currSeqId :: !SequenceId
    } deriving (Eq, Show)


data SequenceIdErrorType
    = SequenceIdDropped
    | SequenceIdDuplicated
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- | If the current sequence ID is greater than 1 more than the last sequence ID then the appropriate error is returned.
checkSeqIdM :: Monad m => SequenceId -- ^ Current sequence ID
            -> (SequenceIdT m) (Maybe SequenceIdError)
checkSeqIdM currSeq = do
    lastSeq <- get
    put $ max lastSeq currSeq
    return $ checkSeqId lastSeq currSeq


------------------------------------------------------------------------------
-- | If the difference between the sequence IDs is not 1 then the appropriate error is returned.
checkSeqId :: SequenceId -- ^ Last sequence ID
           -> SequenceId -- ^ Current sequence ID
           -> Maybe SequenceIdError
checkSeqId lastSeq currSeq
    | delta lastSeq currSeq > 1 = Just $ SequenceIdError SequenceIdDropped    lastSeq currSeq
    | delta lastSeq currSeq < 1 = Just $ SequenceIdError SequenceIdDuplicated lastSeq currSeq
    | otherwise                 = Nothing


delta :: SequenceId -> SequenceId -> Integer
delta lastSeq currSeq = toInteger currSeq - toInteger lastSeq


------------------------------------------------------------------------------
-- | Update to the next sequense ID
nextSeqIdM :: Monad m => SequenceIdT m SequenceId -- ^ Next sequence ID
nextSeqIdM = modify' nextSeqId >> get


------------------------------------------------------------------------------
-- | Update to the next sequense ID
nextSeqId :: SequenceId -- ^ Last sequence ID
          -> SequenceId -- ^ Next sequence ID
nextSeqId = (+1)
