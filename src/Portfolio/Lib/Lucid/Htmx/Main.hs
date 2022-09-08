module Portfolio.Lib.Lucid.Htmx.Main
  ( hxBoost_,
    hxConfirm_,
    hxEncoding_,
    hxExt_,
    hxDelete_,
    hxDisable_,
    hxGet_,
    hxHeaders_,
    hxHistoryElt_,
    hxInclude_,
    hxIndicator_,
    hxParams_,
    hxPatch_,
    hxPost_,
    hxPreserve_,
    hxPrompt_,
    hxPushUrl_,
    hxPut_,
    hxRequest_,
    hxSelect_,
    hxSse_,
    hxSwapOob_,
    hxSwap_,
    hxTarget_,
    hxTrigger_,
    hxVals_,
    hxWs_
  ) where

import RIO
import qualified Lucid
import qualified Lucid.Base

hxBoost_ :: Text -> Lucid.Attribute
hxBoost_ = Lucid.Base.makeAttribute "data-hx-boost"

hxConfirm_ :: Text -> Lucid.Attribute
hxConfirm_ = Lucid.Base.makeAttribute "data-hx-confirm"

hxDelete_ :: Text -> Lucid.Attribute
hxDelete_ = Lucid.Base.makeAttribute "data-hx-delete"

hxDisable_ :: Lucid.Attribute
hxDisable_ = Lucid.Base.makeAttribute "data-hx-disable" mempty

hxEncoding_ :: Text -> Lucid.Attribute
hxEncoding_ = Lucid.Base.makeAttribute "data-hx-encoding"

hxExt_ :: Text -> Lucid.Attribute
hxExt_ = Lucid.Base.makeAttribute "data-hx-ext"

hxGet_ :: Text -> Lucid.Attribute
hxGet_ = Lucid.Base.makeAttribute "data-hx-get"

hxHeaders_ :: Text -> Lucid.Attribute
hxHeaders_ = Lucid.Base.makeAttribute "data-hx-headers"

hxHistoryElt_ :: Lucid.Attribute
hxHistoryElt_ = Lucid.Base.makeAttribute "data-hx-history-elt" mempty

hxInclude_ :: Text -> Lucid.Attribute
hxInclude_ = Lucid.Base.makeAttribute "data-hx-include"

hxIndicator_ :: Text -> Lucid.Attribute
hxIndicator_ = Lucid.Base.makeAttribute "data-hx-indicator"

hxParams_ :: Text -> Lucid.Attribute
hxParams_ = Lucid.Base.makeAttribute "data-hx-params"

hxPatch_ :: Text -> Lucid.Attribute
hxPatch_ = Lucid.Base.makeAttribute "data-hx-patch"

hxPost_ :: Text -> Lucid.Attribute
hxPost_ = Lucid.Base.makeAttribute "data-hx-post"

hxPreserve_ :: Text -> Lucid.Attribute
hxPreserve_ = Lucid.Base.makeAttribute "data-hx-preserve"

hxPrompt_ :: Text -> Lucid.Attribute
hxPrompt_ = Lucid.Base.makeAttribute "data-hx-prompt"

hxPushUrl_ :: Text -> Lucid.Attribute
hxPushUrl_ = Lucid.Base.makeAttribute "data-hx-push-url"

hxPut_ :: Text -> Lucid.Attribute
hxPut_ = Lucid.Base.makeAttribute "data-hx-put"

hxRequest_ :: Text -> Lucid.Attribute
hxRequest_ = Lucid.Base.makeAttribute "data-hx-request"

hxSelect_ :: Text -> Lucid.Attribute
hxSelect_ = Lucid.Base.makeAttribute "data-hx-select"

hxSse_ :: Text -> Lucid.Attribute
hxSse_ = Lucid.Base.makeAttribute "data-hx-sse"

hxSwapOob_ :: Text -> Lucid.Attribute
hxSwapOob_ = Lucid.Base.makeAttribute "data-hx-swap-oob"

hxSwap_ :: Text -> Lucid.Attribute
hxSwap_ = Lucid.Base.makeAttribute "data-hx-swap"

hxTarget_ :: Text -> Lucid.Attribute
hxTarget_ = Lucid.Base.makeAttribute "data-hx-target"

hxTrigger_ :: Text -> Lucid.Attribute
hxTrigger_ = Lucid.Base.makeAttribute "data-hx-trigger"

hxVals_ :: Text -> Lucid.Attribute
hxVals_ = Lucid.Base.makeAttribute "data-hx-vals"

hxWs_ :: Text -> Lucid.Attribute
hxWs_ = Lucid.Base.makeAttribute "data-hx-ws"

