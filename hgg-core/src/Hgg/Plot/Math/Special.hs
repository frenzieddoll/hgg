-- |
-- Module      : Hgg.Plot.Math.Special
-- Description : 特殊関数 (log-gamma / 正則化不完全ベータ / ベータ分位点)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- backend 非依存の数値特殊関数。 確率プロットの厳密 rank-based CI
-- (順序統計量 U_(i) ~ Beta(i, n-i+1)) などで必要になる:
--
--   * 'logGamma'          : ln Γ(x) (Lanczos 近似、 x > 0)
--   * 'regIncompleteBeta' : 正則化不完全ベータ I_x(a,b) (連分数 / Lentz 法)
--   * 'betaQuantile'      : I_x(a,b) = q を満たす x (二分法による逆関数)
--
-- アルゴリズムは Numerical Recipes の @gammln@ / @betai@ / @betacf@ に準ずる。
-- core 内に他の特殊関数 (invNormCdf) は 'Hgg.Plot.Layout.RangeOf' にあるが、
-- ベータ系はサイズが大きいので本 module に分離する。
module Hgg.Plot.Math.Special
  ( logGamma
  , regIncompleteBeta
  , betaQuantile
  ) where

-- ===========================================================================
-- log-gamma (Lanczos 近似、 g=5 / 6 係数)
-- ===========================================================================

-- | ln Γ(x) (x > 0 を仮定)。 相対誤差 < 2e-10。
-- Numerical Recipes @gammln@ と同一係数 (Lanczos, g=5)。
logGamma :: Double -> Double
logGamma x =
  let tmp0 = x + 5.5
      tmp  = tmp0 - (x + 0.5) * log tmp0
      ser  = 1.000000000190015
             + sum [ c / (x + fromIntegral j) | (j, c) <- zip [(1 :: Int) ..] cof ]
  in -tmp + log (2.5066282746310005 * ser / x)
  where
    cof = [  76.18009172947146,   -86.50532032941677
          ,  24.01409824083091,    -1.231739572450155
          ,   0.1208650973866179e-2, -0.5395239384953e-5 ]

-- ===========================================================================
-- 正則化不完全ベータ I_x(a,b)
-- ===========================================================================

-- | 正則化不完全ベータ関数 I_x(a,b) = B(x;a,b) / B(a,b) ∈ [0,1]。
-- a,b > 0、 x ∈ [0,1]。 x < (a+1)/(a+b+2) で連分数を直接、 それ以外は
-- 対称性 I_x(a,b) = 1 - I_{1-x}(b,a) を使い収束を確保する。
regIncompleteBeta :: Double -> Double -> Double -> Double
regIncompleteBeta a b x
  | x <= 0    = 0
  | x >= 1    = 1
  | otherwise =
      let bt = exp ( logGamma (a + b) - logGamma a - logGamma b
                     + a * log x + b * log (1 - x) )
      in if x < (a + 1) / (a + b + 2)
           then bt * betacf a b x / a
           else 1 - bt * betacf b a (1 - x) / b

-- | I_x(a,b) の連分数展開 (Lentz の修正法)。 NR @betacf@ と同型。
betacf :: Double -> Double -> Double -> Double
betacf a b x = go 1 h0 c0 d0
  where
    fpmin = 1e-30
    eps   = 3e-12
    maxit = 300 :: Int
    qab = a + b
    qap = a + 1
    qam = a - 1
    fix v = if abs v < fpmin then fpmin else v
    d0 = 1 / fix (1 - qab * x / qap)
    c0 = 1
    h0 = d0
    go m h c d
      | m > maxit = h
      | abs (del - 1) < eps = h2
      | otherwise = go (m + 1) h2 c2 d2
      where
        m2  = fromIntegral (2 * m) :: Double
        fm  = fromIntegral m :: Double
        -- 偶数ステップ
        aa1 = fm * (b - fm) * x / ((qam + m2) * (a + m2))
        d1  = 1 / fix (1 + aa1 * d)
        c1  = fix (1 + aa1 / c)
        h1  = h * d1 * c1
        -- 奇数ステップ
        aa2 = negate (a + fm) * (qab + fm) * x / ((a + m2) * (qap + m2))
        d2  = 1 / fix (1 + aa2 * d1)
        c2  = fix (1 + aa2 / c1)
        del = d2 * c2
        h2  = h1 * del

-- ===========================================================================
-- ベータ分位点 (I_x(a,b) = q の逆関数)
-- ===========================================================================

-- | I_x(a,b) = q を満たす x ∈ [0,1] を二分法で求める (= Beta(a,b) の q 分位点)。
-- 'regIncompleteBeta' は x について単調増加なので二分法が確実に収束する。
-- 80 反復で区間幅は 2^-80 (≈ 1e-24) になり double 精度では完全収束。
betaQuantile :: Double -> Double -> Double -> Double
betaQuantile q a b
  | q <= 0    = 0
  | q >= 1    = 1
  | otherwise = bisect 0 1 (80 :: Int)
  where
    bisect lo hi n
      | n <= 0    = mid
      | regIncompleteBeta a b mid < q = bisect mid hi (n - 1)
      | otherwise = bisect lo mid (n - 1)
      where mid = (lo + hi) / 2
