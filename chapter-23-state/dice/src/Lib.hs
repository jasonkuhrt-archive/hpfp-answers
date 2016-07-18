module Lib where



import qualified System.Random as Random

data Die =
  DieOne |
  DieTwo |
  DieThree |
  DieFour |
  DieFive |
  DieSix
  deriving (Show, Eq)



intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
intToDie n = error $ "Unable to convert given int (\""++ show n ++"\") into a die"



rollThree :: Int -> (Die,Die,Die)
rollThree seed = (intToDie int1, intToDie int2, intToDie int3)
  where
  (int1, gen2) = Random.randomR (1,6) (Random.mkStdGen seed) :: (Int, Random.StdGen)
  (int2, gen3) = Random.randomR (1,6) gen2 :: (Int, Random.StdGen)
  (int3, _   ) = Random.randomR (1,6) gen3
