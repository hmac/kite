module ExpandExports
  ( expandExports
  )
where

-- Given a module, makes all exports explicit by enumerating the top level decls
-- (unless the module already specifies an export list).

import           Syn

expandExports :: Module -> Module
expandExports modul | not (null (moduleExports modul)) = modul
                    | otherwise = modul { moduleExports = topLevelDecls modul }

topLevelDecls :: Module -> [(RawName, [RawName])]
topLevelDecls modul =
  let datas = [ (dataName d, map conName (dataCons d)) | d <- dataDecls modul ]
      funs  = [ (funName f, []) | f <- funDecls modul ]
  in  datas <> funs
