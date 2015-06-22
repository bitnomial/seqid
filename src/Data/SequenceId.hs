{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SequenceId
       ( checkSeqId
       , incrementSeqId

         -- * Monadic
       , checkSeqIdM
       , incrementSeqIdM
       , lastSeqIdM
       , SequenceIdT (..)
       , runSequenceIdT
       , execSequenceIdT
       , evalSequenceIdT

         -- * Types
       , SequenceIdError (..)
       , SequenceIdErrorType (..)
       , SequenceId (..)
       ) where


import           Control.Applicative       (Applicative)
import           Control.Monad.State.Class (MonadState, get, modify', put)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.State (StateT (..), evalStateT, execStateT)
import           Data.Word                 (Word32)


newtype SequenceIdT m a = SequenceIdT { unSequenceIdT :: StateT SequenceId m a }
                        deriving (Monad, Applicative, Functor, MonadState SequenceId, MonadTrans)

newtype SequenceId = SequenceId { unSequenceId :: Word32 }
                   deriving (Show, Eq, Ord, Num, Integral, Real, Enum)


evalSequenceIdT :: Monad m => SequenceIdT m b -> SequenceId -> m b
evalSequenceIdT = evalStateT . unSequenceIdT


execSequenceIdT :: Monad m => SequenceIdT m b -> SequenceId -> m SequenceId
execSequenceIdT = execStateT . unSequenceIdT


runSequenceIdT :: Monad m => SequenceIdT m b -> SequenceId -> m (b, SequenceId)
runSequenceIdT = runStateT . unSequenceIdT


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


-- | If the current sequence ID is greater than 1 more than the last
-- sequence ID then the appropriate error is returned.
checkSeqIdM :: Monad m => SequenceId -- ^ Current sequence ID
            -> (SequenceIdT m) (Maybe SequenceIdError)
checkSeqIdM currSeq = do
    lastSeq <- get
    put $ max lastSeq currSeq
    return $ checkSeqId lastSeq currSeq


-- | If the difference between the sequence IDs is not 1 then the
-- appropriate error is returned.
checkSeqId :: SequenceId -- ^ Last sequence ID
           -> SequenceId -- ^ Current sequence ID
           -> Maybe SequenceIdError
checkSeqId lastSeq currSeq
    | delta lastSeq currSeq > 1 = Just $ SequenceIdError SequenceIdDropped    lastSeq currSeq
    | delta lastSeq currSeq < 1 = Just $ SequenceIdError SequenceIdDuplicated lastSeq currSeq
    | otherwise                 = Nothing


delta :: SequenceId -> SequenceId -> Int
delta lastSeq currSeq = fromIntegral currSeq - fromIntegral lastSeq


-- | Update to the next sequense ID
incrementSeqIdM :: Monad m => SequenceIdT m SequenceId -- ^ Next sequence ID
incrementSeqIdM = modify' incrementSeqId >> get


-- | Increment to the next sequense ID
incrementSeqId :: SequenceId -- ^ Last sequence ID
               -> SequenceId -- ^ Next sequence ID
incrementSeqId = (+1)


-- | Last seen sequense ID
lastSeqIdM :: Monad m => SequenceIdT m SequenceId -- ^ Last sequence ID
lastSeqIdM = get
