module Halogen.SVG
  -- ( svgElement
  -- , svg
  -- , circle

  -- , viewBox
  -- , cx
  -- , cy
  -- , r
  -- , fill
  -- )
  where

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

svgNS = A.prop (A.attributeName "namespace") "http://www.w3.org/2000/svg"

svgElement :: forall i. H.TagName -> [A.Attr i] -> [H.HTML i] -> H.HTML i
svgElement tagName attrs = H.Element tagName (svgNS : attrs)

svg :: forall i. [A.Attr i] -> [H.HTML i] -> H.HTML i
svg = svgElement (H.tagName "svg")

circle :: forall i. [A.Attr i] -> [H.HTML i] -> H.HTML i
circle = svgElement (H.tagName "circle")

g :: forall i. [A.Attr i] -> [H.HTML i] -> H.HTML i
g = svgElement (H.tagName "g")

path :: forall i. [A.Attr i] -> [H.HTML i] -> H.HTML i
path = svgElement (H.tagName "path")


cx :: forall i. String -> A.Attr i
cx = A.attr $ A.attributeName "cx"

cy :: forall i. String -> A.Attr i
cy = A.attr $ A.attributeName "cy"

d :: forall i. String -> A.Attr i
d = A.attr $ A.attributeName "d"

fill :: forall i. String -> A.Attr i
fill = A.attr $ A.attributeName "fill"

r :: forall i. String -> A.Attr i
r = A.attr $ A.attributeName "r"

transform :: forall i. String -> A.Attr i
transform = A.attr $ A.attributeName "transform"

viewBox :: forall i. String -> A.Attr i
viewBox = A.attr $ A.attributeName "viewBox"
