import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.Monad (foldM)
import System.Posix.Files (getFileStatus, isDirectory)

main :: IO ()
main = print =<< walk "."

type Dir  = FilePath  -- you might want to use
type File = FilePath  -- stronger types here!

data FileTree = Dir :> [Either File FileTree]
    deriving Show

walk :: Dir -> IO FileTree
walk dir = do
    paths' <- listDirectory dir
    let paths = fmap (dir </>) paths' -- need to qualify paths
    children <- foldM go [] paths
    pure (dir :> children)
  where
    go :: [Either File FileTree] -> FilePath -> IO [Either File FileTree]
    go accum filePath = do
        stat <- getFileStatus filePath
        if isDirectory stat
            then do
                tree <- walk filePath
                pure (Right tree : accum)
            else do
                pure (Left filePath : accum)
