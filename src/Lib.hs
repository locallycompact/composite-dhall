{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Composite.Record
import Composite.TH
import qualified Control.Lens as L
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as T
import qualified Dhall as D
import Dhall.Core hiding (File, Text, field)
import Dhall.Map
import GHC.TypeLits

type Id = Text

type Title = Text

type Url = Text

withLensesAndProxies
  [d|
    type FId = "id" :-> Id

    type FTitle = "title" :-> Title

    type FTags = "tags" :-> [Text]

    type FUrl = "url" :-> Url
    |]

type PageMeta = '[FId, FTitle, FUrl]

instance D.ToDhall (Record '[]) where
  injectWith = pure (D.Encoder {..})
    where
      embed _ = RecordLit mempty
      declared = Record mempty

instance (KnownSymbol s, D.ToDhall (Record xs), D.ToDhall x) => D.ToDhall (Record (s :-> x ': xs)) where
  injectWith = do
    let f :: s :-> x
        f = undefined
    let name = valName f
    let D.Encoder embedL declaredL = D.inject
    let D.Encoder embedR declaredR = D.inject
    let embed (s :*: xs) = RecordLit (Dhall.Map.insert name (Dhall.Core.makeRecordField (embedL $ s)) mapR)
          where
            mapR = _ $ embedR xs
    let declared = Record (Dhall.Map.insert name (Dhall.Core.makeRecordField (declaredL)) mapR)
          where
            mapR = _ declaredR
    pure (D.Encoder {..})

injectRecord :: D.Encoder (Record PageMeta)
injectRecord =
  D.recordEncoder
    ( adapt
        >$< D.encodeField "id"
        D.>*< D.encodeField "title"
        D.>*< D.encodeField "url"
    )
  where
    adapt :: Record PageMeta -> (Id, (Title, Url))
    adapt x = (L.view fId x, (L.view fTitle x, (L.view fUrl x)))

instance D.ToDhall (Record PageMeta) where
  injectWith _ = injectRecord
