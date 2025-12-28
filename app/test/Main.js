import jsdom from "jsdom";


export function mockBrowserImpl() {
  const dom = new jsdom.JSDOM("")
  global.window = dom.window
  global.document = global.window.document
  global.window.SVGElement.prototype.getBBox = () => ({ x: 0, y: 0 });
}
