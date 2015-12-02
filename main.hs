import Fcm
import InputUtils
import System.Console.CmdArgs
import System.Environment


data_args = InputOptions{
  delimiter = "," &= help "Sets csv delimiter",
  stripHeader = False &= help "Strips first csv line",
  stripClass=False &= help "Strips last column",
  stripNumber=False &= help "Strips first column",
  distanceFunction=  enum [Hamming &= help "Use hamming distance function(used by default)" , Euclidean &= help "Use euclidean distance function"],
  clusterCount = 4 &= help "sets cluster count, default is 4",
  fuzziness = 2 &= help "sets fuzziness for cmeans",
  threshold = 0.001 &= help "sets fuzziness for cmeans",
  input = "",
  output = ""
}


main = do
  named_args <- cmdArgs data_args
  input <- parseCSV (input named_args)

  let parsed_data = ($!) applyInputOptions input (stripHeader named_args) (stripClass named_args) (stripNumber named_args)
  let result = separate (distanceFunction named_args) (clusterCount named_args) (fuzziness named_args) (threshold named_args) parsed_data

  case (null $ output named_args) of
    False -> writeFile (output named_args) $ unlines $ map show result
    True -> putStrLn $ unlines $ map show result

