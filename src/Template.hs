module Template
( generateExe ) where

import qualified Data.Map as M
import qualified Data.Text as T
import System.IO

generateExe :: FilePath -> FilePath -> M.Map String String -> IO ()
generateExe templateFile outputFile map = do
  template <- readFile templateFile
  let output = subst map template
  writeFile outputFile output

-- foldr :: (a -> b -> b) -> b -> Map k a -> b
-- foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
subst :: M.Map String String -> String -> String
subst map s = M.foldrWithKey go s map
  where go k new s = T.unpack $ T.replace (T.pack k) (T.pack new) (T.pack s)
