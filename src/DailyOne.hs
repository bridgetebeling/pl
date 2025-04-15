module DailyOne where

-- | Computes a + b*x + c*x^2
quadratic :: Num a => a -> a -> a -> a -> a
quadratic a b c x = a + b * x + c * x ^ 2

-- | Scales a 2D vector by a scalar
scaleVector :: Num a => a -> (a, a) -> (a, a)
scaleVector s (x, y) = (s * x, s * y)

-- | Cartesian distance between two 3D points
tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
tripleDistance (x1, y1, z1) (x2, y2, z2) =
  sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
