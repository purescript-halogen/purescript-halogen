module Example.Hydration.MainGenerateIndexHtml where

import Prelude

import Effect (Effect)
import Example.Hydration.App as App
import Halogen.VDom.StringRenderer.RenderComponent (renderComponent)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)

main :: Effect Unit
main = do
  let rendered = renderComponent App.component unit
  let
    html =
      """<!doctype html>
<html>
<head>

<title>Halogen Example - Higher Order Components</title>
<style>

body {
  font-family: sans-serif;
  max-width: 800px;
  margin: auto;
}

.Panel {
  border: 1px solid #ccc;
  border-radius: 2px;
  margin: 1em;
  border-radius: 0.2em;
}

.Panel--open {
  border-radius: 0.2em 0.2em 0.5em 0.5em;
}

.Panel-header {
  background: #ddd;
  padding: 0.5em;
  text-align: right;
}

.Panel-content {
  padding: 0.5em;
}

</style>

</head>
<body><div id="root">""" <> rendered <>
        """</div>
  <script src="example.js"></script>
</body>
</html>"""
  writeTextFile UTF8 "./dist/index.html" html
