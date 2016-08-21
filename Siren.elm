module Siren exposing (..)

import Json exposing (JsonVal)

type alias SirenLinkRel = String
type alias SirenLinkRels = List SirenLinkRel
type alias SirenHref = String

type alias SirenLink =
  { rel: SirenLinkRels,
    href: SirenHref }

type alias SirenProperties = JsonVal

type alias SirenField =
  { name: String,
    type': Maybe String,
    value: Maybe String,
    title: Maybe String }

type alias SirenFields = List SirenField

type alias SirenAction =
  { name: String
  , class: Maybe SirenClasses
  , method: Maybe String
  , href: SirenHref
  , title: Maybe String
  , fields: Maybe SirenFields }

type alias SirenActions = List SirenAction

type alias SirenClass = String

type alias SirenClasses = List SirenClass

type alias SirenLinks = List SirenLink

type alias SirenDocument =
  { class: Maybe SirenClasses
  , properties: Maybe SirenProperties
  , actions: Maybe SirenActions
  , links: Maybe SirenLinks }
