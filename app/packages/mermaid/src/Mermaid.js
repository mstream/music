let mermaid = null

async function getRender() {
  if (!mermaid) {
    mermaid = (await import("mermaid")).default;
  }
  return mermaid.render
}

async function renderDiagramSvg(diagramDef) {
  try {
    const elId = "mermaid-dummy"
    const el = document.createElement('pre');
    el.setAttribute("id", elId);
    const render = await getRender()
    const { svg } = await render(elId, diagramDef);
    el.remove()
    return svg
  } catch (error) {
    throw error
  }
}

export function renderImpl(diagramDef) {
  return function() {
    return renderDiagramSvg(diagramDef)
  };
}
