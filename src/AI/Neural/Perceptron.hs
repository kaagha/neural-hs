{- |
  Module      :  AI.Neural.Perceptron
  Copyright   :  (c) Karim Agha 2016
  License     :  MIT
  Maintainer  :  <karim.dev@gmail.com>
  Stability   :  experimental
  Portability :  portable
  -}

module AI.Neural.Perceptron where

    
    -- sigmoid neuron
    sigmoid :: Float -> Float
    sigmoid x = 1 / (1 + exp (-1 * x))