module Madlib where



-- import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclaimation = String



madlib :: Exclaimation -> Adverb -> Noun -> Adjective -> String
madlib exc adv nou adj = mconcat [
  exc,
  "! he said ",
  adv,
  " as he jumped into his car ",
  nou,
  " and drove off with this ",
  adj,
  " wife."
  ]
