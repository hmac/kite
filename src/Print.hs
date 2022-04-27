module Print where

import           Prettyprinter

-- Semantic annotation
data Style = VarStyle | KeywordStyle | FunctionStyle | TypeStyle | DataStyle | HoleStyle

var :: Document -> Document
var = annotate VarStyle

keyword :: Document -> Document
keyword = annotate KeywordStyle

func :: Document -> Document
func = annotate FunctionStyle

type_ :: Document -> Document
type_ = annotate TypeStyle

data_ :: Document -> Document
data_ = annotate DataStyle

hole :: Document -> Document
hole = annotate HoleStyle

type Document = Doc Style
