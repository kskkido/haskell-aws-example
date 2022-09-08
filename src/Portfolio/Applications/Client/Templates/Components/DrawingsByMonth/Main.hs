module Portfolio.Applications.Client.Templates.Components.DrawingsByMonth.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.Time.Calendar as Time.Calendar
import qualified Portfolio.Lib.Day.Main as Lib.Day
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Applications.Client.Templates.Components.Drawing.Main as Components.Drawing
import qualified Portfolio.Applications.Client.Data.Drawing.Main as Drawing

render :: Monad m => [(Time.Calendar.Day, [Drawing.Drawing])] -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render drawingsByMonth attributes = do
  Lucid.div_ ([] `Lib.Lucid.Attribute.concat` attributes) do
    flip Foldable.foldMap drawingsByMonth $ \(month, drawings) -> do
      Lucid.div_ [] do
        Lucid.h3_ [Lucid.classes_ ["mb-6"]] do
          Lucid.toHtml $ Lib.Day.toMonthString month
        Lucid.div_ [Lucid.classes_ ["grid", "grid-cols-3", "gap-x-8", "gap-y-24"]] do
          flip Foldable.foldMap drawings $ \drawing -> do
            Lucid.div_ [Lucid.classes_ ["relative"]] do
              Lucid.div_ [Lucid.classes_ ["relative"]] do
                Lucid.div_ [Lucid.classes_ ["block", "pb-[100%]"]] mempty
                Components.Drawing.render drawing
                  [ Lucid.classes_ ["absolute", "inset-0", "m-auto", "w-full", "h-full", "object-cover", "cursor-pointer"]
                  ]
              Lucid.div_ [Lucid.classes_ ["relative", "py-2"]] do
                Lucid.span_ [Lucid.classes_ []] do
                  Lucid.toHtml drawing.title
