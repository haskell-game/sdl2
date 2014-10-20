{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module SDL.Hints (Hint(..), RenderScaleQuality(..), setHint) where

import Foreign
import Foreign.C
import qualified SDL.Raw as Raw

data RenderScaleQuality = ScaleNearest | ScaleLinear | ScaleBest

data Hint :: * -> * where
  HintRenderScaleQuality :: Hint RenderScaleQuality

setHint :: Hint v -> v -> IO Bool
setHint (HintRenderScaleQuality) v =
  withCString "SDL_RENDER_SCALE_QUALITY" $ \hint ->
    withCString
      (case v of
         ScaleNearest -> "0"
         ScaleLinear -> "1"
         ScaleBest -> "2")
      (Raw.setHint hint)
