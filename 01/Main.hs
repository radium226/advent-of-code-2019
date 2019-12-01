import System.Environment

readModuleMasses :: FilePath -> IO [Int]
readModuleMasses filePath = do
  fileContent <- readFile filePath
  let fileLines = lines fileContent
  return (map (read :: String -> Int) fileLines)

moduleFuelRequirement :: Int -> Int
moduleFuelRequirement mass = (floor ((fromIntegral mass) / 3)) - 2


computeModuleFuelRequirement :: Int -> Int
computeModuleFuelRequirement mass
  | fuelRequirement <= 0 = 0
  | otherwise            = (fuelRequirement + (computeModuleFuelRequirement fuelRequirement))
  where fuelRequirement = (floor ((fromIntegral mass) / 3)) - 2


main :: IO ()
main = do
  [inputFilePath]  <- getArgs
  moduleMasses <- readModuleMasses inputFilePath
  let moduleFuelRequirements = map computeModuleFuelRequirement moduleMasses
  let spaceshipFuelRequirements = sum moduleFuelRequirements
  putStrLn (show spaceshipFuelRequirements)
