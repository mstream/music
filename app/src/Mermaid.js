import mermaid from "mermaid";

async function renderDiagramSvg(diagramDef) {
  const elId = "mermaid-tmp"
  const el = document.createElement('pre');
  el.setAttribute("id", elId);
  const { svg } = await mermaid.render(elId, diagramDef);
  el.remove()
  return svg
}

export function renderImpl(diagramDef) {
  return function() {
    return renderDiagramSvg(diagramDef)
  };
}

