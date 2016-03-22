{- |
  Module      :  AI.Neural
  Copyright   :  (c) Karim Agha 2016
  License     :  MIT
  Maintainer  :  <karim.dev@gmail.com>
  Stability   :  experimental
  Portability :  portable
  -}

module AI.Neural where

    import Data.List.Split

    type Signal = Float
    type NeuralMemory = [[[Signal]]]
    type FeatureVector = [Signal]
    type NeuralFunction = (Signal -> Signal)
    type NeuralLayer = [NeuralFunction]
    type NeuralNetwork = [NeuralLayer]

    -- neural topology of the neural network
    topology :: NeuralNetwork -> [Int]
    topology = map length

    -- connections topology of the axons between layers
    connections :: NeuralNetwork -> [Int]
    connections n = zipWith (*) (init t) (tail t)
        where
            t = topology n

    -- given a contigous and flat list of floats that represent the neural
    -- network memory, this function will partition that list into a list
    -- of lists of lists, where the first level maps to the layers of the
    -- network, and the second maps to the list of weights of inputs to a
    -- single neuron within the layer.
    partition :: [Signal] -> NeuralNetwork -> NeuralMemory
    partition m ls = map neurons $ zip ls layers
        where
            layers = splitPlaces (connections ls) m            
            neurons l = chunksOf (length $ fst l) (snd l)

    -- a forward pass on the network
    --  xs : the input feature vector
    --  m  : the memory of the network
    --  ls : the structure/layers of the network
    -- outputs a feature fector of the last layer
    --forward :: FeatureVector -> NeuralMemory -> NeuralNetwork -> FeatureVector
    forward xs m ls = activateLayer (snd inputLayer) xs
        where
            weight ws xs = zipWith (*) ws xs
            activate fs xs = zipWith ($) fs xs
            partitionedMemory = partition m ls
            adjacentLayers = zip (init ls) (tail ls)     
            layers = zip ls m
            inputLayer = (head partitionedMemory, head ls)
            activateLayer fs xs = zipWith ($) fs xs
