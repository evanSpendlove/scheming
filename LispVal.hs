{-# LANGUAGE OverloadedStrings #-}
import Data.Text as T

data LispVal
 = Atom T.Text
 | List [LispVal]
 | Number Integer
 | String T.Text
 | Fun IFunc
 | Lambda IFunc EnvCtx
 | Nil
 | Bool Bool deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
