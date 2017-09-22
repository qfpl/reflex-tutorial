module Solutions (
    solutions
  ) where

import Language.Javascript.JSaddle (JSM)

import Ex00.Solution
import Ex01.Solution
import Ex02.Solution
import Ex03.Solution
import Ex04.Solution
import Ex05.Solution
import Ex06.Solution
import Ex07.Solution
import Ex08.Solution

solutions :: JSM ()
solutions = do
  attachEx00
  attachEx01
  attachEx02
  attachEx03
  attachEx04
  attachEx05
  attachEx06
  attachEx07
  attachEx08
