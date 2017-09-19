module Solutions (
    solutions
  ) where

import Language.Javascript.JSaddle (JSM)

import Ex00.Solution
import Ex01.Solution

solutions :: JSM ()
solutions = do
  attachEx00
  attachEx01
