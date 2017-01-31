{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module SDL.Exception
  ( SDLException(..)
  ) where

import Control.Exception
import Data.Data (Data)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Error details about a failure to call an SDL routine. Almost all functions in this library have the
-- ability to produce exceptions of this type. Inspection should help you learn more about what has
-- gone wrong.
data SDLException
  = -- | A call to a low-level SDL C function failed unexpectedly.
    SDLCallFailed
    {sdlExceptionCaller :: !Text
     -- ^ The Haskell routine that was trying to call a C function
    ,sdlFunction :: !Text
     -- ^ The C function that was called and produced an error
    ,sdlExceptionError :: !Text
    -- ^ SDL's understanding of what has gone wrong
    }
  | -- | An SDL C function was called with an unexpected argument.
    SDLUnexpectedArgument
    {sdlExceptionCaller :: !Text
     -- ^ The Haskell routine that was trying to call a C function
    ,sdlFunction :: !Text
     -- ^ The C function that was called and produced an error
    ,sdlUnknownValue :: !String
     -- ^ The argument that SDL failed to understand
    }
  | -- | A hint was attempted to be set, but SDL does not know about this hint.
    SDLUnknownHintValue
    {sdlHint :: !String
     -- ^ The hint that could not be set
    ,sdlUnknownValue :: !String
     -- ^ The value that could not be set
    }
  deriving (Data,Eq,Generic,Ord,Read,Show,Typeable)

instance Exception SDLException
