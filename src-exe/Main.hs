import AntiEntropy
import Algebra.Lattice.Ordered
import Control.Monad (when)
import Options.Applicative
import Network.Socket (PortNumber(..))

import System.Log.Logger

data NodeSettings = NodeSettings { port :: PortNumber
                                 , debug :: Bool
                                 , neighborURLs :: [String]
                                 }

nodeParser :: Parser NodeSettings
nodeParser = NodeSettings
    <$> option auto
        ( long "port"
        <> metavar "INT"
        <> value 4400
        <> help "The port that the program runs on")
    <*> switch
        ( long "debug"
        <> short 'd'
        <> help "When set, prints debug information")
    <*> many (strArgument
        ( metavar "ADDRESS"
        <> help "Neighbor Addresses"))

programSettings :: ParserInfo NodeSettings
programSettings = info (helper <*> nodeParser)
    (fullDesc
    <> progDesc "Runs a server node that replicates data across all nodes in the system"
    <> header "Conflict Free Replicated Data Types (CRDT)")

main :: IO ()
main = do
    settings <- execParser programSettings
    when (debug settings) $ updateGlobalLogger "" (setLevel DEBUG)
    runNode (Ordered (0::Int)) (port settings) (neighborURLs settings)
