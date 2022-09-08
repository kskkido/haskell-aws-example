module Portfolio.Applications.Client.Data.Page.Main
  ( Page(..)
  ) where

import qualified Portfolio.Applications.Client.Data.PageContext.Main as PageContext

data Page a = Page
  { context :: PageContext.PageContext
  , html :: a
  }

