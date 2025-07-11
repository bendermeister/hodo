module Cli.List
  ( run,
  )
where

import Context (Context)
import Context qualified hiding (Context)

run :: Context -> [String] -> Context
run context _ = Context.println context "running list"
