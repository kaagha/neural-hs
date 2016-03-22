
module Main where

    import AI.Neural
    import AI.Neural.Perceptron

    import Control.Monad.Random


    -- the neurons topology of the neural network
    structure :: NeuralNetwork
    structure = [replicate 784 id,
                 replicate 500 sigmoid,
                 replicate 300 sigmoid,
                 replicate 10  sigmoid];

    -- the neural memory in its zero/blank state at T0
    zerobuffer :: [Signal]
    zerobuffer = let size = sum $ connections structure
        in replicate size 0

    -- [debug] generate a random feature vector of 784 values between 0 and 1
    randomInput :: NeuralNetwork -> FeatureVector
    randomInput s = take inputSize generator
        where
            inputSize = head $ topology s
            generator = randoms (mkStdGen 3) :: [Signal]


    input :: FeatureVector
    input = randomInput structure


    main = do
        print $ "Neural"
        print $ "topology = " ++ show (topology structure)
        print $ "connections = " ++ show (connections structure)
        print $ "input size = " ++ show (length $ randomInput structure)
        print $ "forward = " ++ show (forward input zerobuffer structure)