module Test.Mermaid.Main (main) where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Mermaid (render) as Mermaid
import Mermaid.DiagramDef (DiagramDef)
import Mermaid.DiagramDef.Blocks.BlockDef (BlockDef(..))
import Random.LCG (mkSeed)
import Test.Mermaid.DiagramDef as DiagramDef
import Test.Mermaid.DiagramDef.Blocks.BlockDef.Unsafe
  ( unsafeGroupBlockChildren
  )
import Test.Mermaid.DiagramDef.Unsafe (unsafeBlockDiagramDef)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (evalGen, vectorOf) as Gen
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Utils (lines)

foreign import mockBrowserImpl ∷ Effect Unit

main ∷ Effect Unit
main = do
  mockBrowserImpl
  runSpecAndExitProcess [ consoleReporter ] spec

spec ∷ Spec Unit
spec = do
  DiagramDef.spec
  renderingTestSuite
    { examples: Map.fromFoldable
        [ unsafeBlockDiagramDef
            { children: unsafeGroupBlockChildren
                [ "node1" /\ (Node "node1 contents" /\ []) ]
            , properties: { columns: Nothing }
            , spacedOut: false
            } /\
            { codeLines:
                [ "block"
                , "  node1[\"node1 contents\"]"
                ]
            , html:
                "<svg id=\"mermaid-dummy\" width=\"100%\" xmlns=\"http://www.w3.org/2000/svg\" style=\"max-width: 10px;\" viewBox=\"-5 -5 10 10\" role=\"graphics-document document\" aria-roledescription=\"block\"><style>#mermaid-dummy{font-family:\"trebuchet ms\",verdana,arial,sans-serif;font-size:16px;fill:#333;}@keyframes edge-animation-frame{from{stroke-dashoffset:0;}}@keyframes dash{to{stroke-dashoffset:0;}}#mermaid-dummy .edge-animation-slow{stroke-dasharray:9,5!important;stroke-dashoffset:900;animation:dash 50s linear infinite;stroke-linecap:round;}#mermaid-dummy .edge-animation-fast{stroke-dasharray:9,5!important;stroke-dashoffset:900;animation:dash 20s linear infinite;stroke-linecap:round;}#mermaid-dummy .error-icon{fill:#552222;}#mermaid-dummy .error-text{fill:#552222;stroke:#552222;}#mermaid-dummy .edge-thickness-normal{stroke-width:1px;}#mermaid-dummy .edge-thickness-thick{stroke-width:3.5px;}#mermaid-dummy .edge-pattern-solid{stroke-dasharray:0;}#mermaid-dummy .edge-thickness-invisible{stroke-width:0;fill:none;}#mermaid-dummy .edge-pattern-dashed{stroke-dasharray:3;}#mermaid-dummy .edge-pattern-dotted{stroke-dasharray:2;}#mermaid-dummy .marker{fill:#333333;stroke:#333333;}#mermaid-dummy .marker.cross{stroke:#333333;}#mermaid-dummy svg{font-family:\"trebuchet ms\",verdana,arial,sans-serif;font-size:16px;}#mermaid-dummy p{margin:0;}#mermaid-dummy .label{font-family:\"trebuchet ms\",verdana,arial,sans-serif;color:#333;}#mermaid-dummy .cluster-label text{fill:#333;}#mermaid-dummy .cluster-label span,#mermaid-dummy p{color:#333;}#mermaid-dummy .label text,#mermaid-dummy span,#mermaid-dummy p{fill:#333;color:#333;}#mermaid-dummy .node rect,#mermaid-dummy .node circle,#mermaid-dummy .node ellipse,#mermaid-dummy .node polygon,#mermaid-dummy .node path{fill:#ECECFF;stroke:#9370DB;stroke-width:1px;}#mermaid-dummy .flowchart-label text{text-anchor:middle;}#mermaid-dummy .node .label{text-align:center;}#mermaid-dummy .node.clickable{cursor:pointer;}#mermaid-dummy .arrowheadPath{fill:#333333;}#mermaid-dummy .edgePath .path{stroke:#333333;stroke-width:2.0px;}#mermaid-dummy .flowchart-link{stroke:#333333;fill:none;}#mermaid-dummy .edgeLabel{background-color:rgba(232,232,232, 0.8);text-align:center;}#mermaid-dummy .edgeLabel rect{opacity:0.5;background-color:rgba(232,232,232, 0.8);fill:rgba(232,232,232, 0.8);}#mermaid-dummy .labelBkg{background-color:rgba(232, 232, 232, 0.5);}#mermaid-dummy .node .cluster{fill:rgba(255, 255, 222, 0.5);stroke:rgba(170, 170, 51, 0.2);box-shadow:rgba(50, 50, 93, 0.25) 0px 13px 27px -5px,rgba(0, 0, 0, 0.3) 0px 8px 16px -8px;stroke-width:1px;}#mermaid-dummy .cluster text{fill:#333;}#mermaid-dummy .cluster span,#mermaid-dummy p{color:#333;}#mermaid-dummy div.mermaidTooltip{position:absolute;text-align:center;max-width:200px;padding:2px;font-family:\"trebuchet ms\",verdana,arial,sans-serif;font-size:12px;background:hsl(80, 100%, 96.2745098039%);border:1px solid #aaaa33;border-radius:2px;pointer-events:none;z-index:100;}#mermaid-dummy .flowchartTitleText{text-anchor:middle;font-size:18px;fill:#333;}#mermaid-dummy .label-icon{display:inline-block;height:1em;overflow:visible;vertical-align:-0.125em;}#mermaid-dummy .node .label-icon path{fill:currentColor;stroke:revert;stroke-width:revert;}#mermaid-dummy :root{--mermaid-font-family:\"trebuchet ms\",verdana,arial,sans-serif;}</style><g></g><marker id=\"mermaid-dummy_block-pointEnd\" class=\"marker block\" viewBox=\"0 0 10 10\" refX=\"6\" refY=\"5\" markerUnits=\"userSpaceOnUse\" markerWidth=\"12\" markerHeight=\"12\" orient=\"auto\"><path d=\"M 0 0 L 10 5 L 0 10 z\" class=\"arrowMarkerPath\" style=\"stroke-width: 1; stroke-dasharray: 1,0;\"></path></marker><marker id=\"mermaid-dummy_block-pointStart\" class=\"marker block\" viewBox=\"0 0 10 10\" refX=\"4.5\" refY=\"5\" markerUnits=\"userSpaceOnUse\" markerWidth=\"12\" markerHeight=\"12\" orient=\"auto\"><path d=\"M 0 5 L 10 10 L 10 0 z\" class=\"arrowMarkerPath\" style=\"stroke-width: 1; stroke-dasharray: 1,0;\"></path></marker><marker id=\"mermaid-dummy_block-circleEnd\" class=\"marker block\" viewBox=\"0 0 10 10\" refX=\"11\" refY=\"5\" markerUnits=\"userSpaceOnUse\" markerWidth=\"11\" markerHeight=\"11\" orient=\"auto\"><circle cx=\"5\" cy=\"5\" r=\"5\" class=\"arrowMarkerPath\" style=\"stroke-width: 1; stroke-dasharray: 1,0;\"></circle></marker><marker id=\"mermaid-dummy_block-circleStart\" class=\"marker block\" viewBox=\"0 0 10 10\" refX=\"-1\" refY=\"5\" markerUnits=\"userSpaceOnUse\" markerWidth=\"11\" markerHeight=\"11\" orient=\"auto\"><circle cx=\"5\" cy=\"5\" r=\"5\" class=\"arrowMarkerPath\" style=\"stroke-width: 1; stroke-dasharray: 1,0;\"></circle></marker><marker id=\"mermaid-dummy_block-crossEnd\" class=\"marker cross block\" viewBox=\"0 0 11 11\" refX=\"12\" refY=\"5.2\" markerUnits=\"userSpaceOnUse\" markerWidth=\"11\" markerHeight=\"11\" orient=\"auto\"><path d=\"M 1,1 l 9,9 M 10,1 l -9,9\" class=\"arrowMarkerPath\" style=\"stroke-width: 2; stroke-dasharray: 1,0;\"></path></marker><marker id=\"mermaid-dummy_block-crossStart\" class=\"marker cross block\" viewBox=\"0 0 11 11\" refX=\"-1\" refY=\"5.2\" markerUnits=\"userSpaceOnUse\" markerWidth=\"11\" markerHeight=\"11\" orient=\"auto\"><path d=\"M 1,1 l 9,9 M 10,1 l -9,9\" class=\"arrowMarkerPath\" style=\"stroke-width: 2; stroke-dasharray: 1,0;\"></path></marker><g class=\"block\"><g class=\"node default default flowchart-label\" id=\"node1\" transform=\"translate(0, 0)\"><rect class=\"basic label-container\" style=\"\" rx=\"0\" ry=\"0\" x=\"0\" y=\"0\" width=\"0\" height=\"0\"></rect><g class=\"label\" style=\"\" transform=\"translate(0, 0)\"><rect></rect><foreignObject width=\"0\" height=\"0\"><div style=\"display: inline-block; white-space: nowrap;\" xmlns=\"http://www.w3.org/1999/xhtml\"><span class=\"nodeLabel\">node1 contents</span></div></foreignObject></g></g></g></svg>"
            }
        ]
    , randomTestCasesQuantity: 25
    }

type RenderingTestSuiteConf =
  { examples ∷
      Map DiagramDef
        { codeLines ∷ Array String, html ∷ String }
  , randomTestCasesQuantity ∷ Int
  }

renderingTestSuite ∷ RenderingTestSuiteConf → Spec Unit
renderingTestSuite conf = describe
  "mermaid diagram rendering"
  (sequence_ testCases)
  where
  testCases ∷ Array (Spec Unit)
  testCases = codeTestCases <> htmlTestCases <> randomTestCases

  codeTestCases ∷ Array (Spec Unit)
  codeTestCases = foldMapWithIndex
    ( \diagramDef codeLines →
        [ exampleCodeTestCase diagramDef codeLines
        ]
    )
    examples
    where
    examples ∷ Map DiagramDef (Array String)
    examples = (_.codeLines) <$> conf.examples

  htmlTestCases ∷ Array (Spec Unit)
  htmlTestCases = foldMapWithIndex
    ( \diagramDef html →
        [ exampleHtmlTestCase diagramDef html
        ]
    )
    examples
    where
    examples ∷ Map DiagramDef String
    examples = (_.html) <$> conf.examples

  randomTestCases ∷ Array (Spec Unit)
  randomTestCases = Array.mapWithIndex randomTestCase randomTestInputs

  randomTestInputs ∷ Array DiagramDef
  randomTestInputs = Gen.evalGen
    (Gen.vectorOf conf.randomTestCasesQuantity arbitrary)
    { newSeed: mkSeed 123, size: 10 }

  exampleCodeTestCase ∷ DiagramDef → (Array String) → Spec Unit
  exampleCodeTestCase diagramDef expectedLines = it "example - code"
    do
      { diagramCode } ← Mermaid.render diagramDef
      diagramCode `shouldEqual` (lines expectedLines)

  exampleHtmlTestCase ∷ DiagramDef → String → Spec Unit
  exampleHtmlTestCase diagramDef expectedHtml = it "example - HTML"
    do
      { diagramHtml } ← Mermaid.render diagramDef
      diagramHtml `shouldEqual` expectedHtml

  randomTestCase ∷ Int → DiagramDef → Spec Unit
  randomTestCase index diagramDef = it
    ("random " <> show index)
    (void $ Mermaid.render diagramDef)

