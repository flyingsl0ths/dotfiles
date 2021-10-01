module LayoutUtils (rawSpacing, rawSpacing') where

import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    spacingRaw,
  )

-- Wrapper around spacingRaw, used to facilitate calls
-- The spacingRaw function adds a configurable amount of space around windows.
rawSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
rawSpacing amount =
  spacingRaw
    False
    (Border amount amount amount amount)
    True
    (Border amount amount amount amount)
    True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
rawSpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
rawSpacing' amount =
  spacingRaw
    True
    (Border amount amount amount amount)
    True
    (Border amount amount amount amount)
    True
