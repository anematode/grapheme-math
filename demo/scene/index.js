let renderer, scene, polyline

function play() {
    renderer = new Grapheme.WebGLRenderer()
    scene = new Grapheme.InteractiveScene()

    document.body.appendChild(scene.domElement)

    polyline = new Grapheme.PolylineElement()
    polyline.setVertices([ 0, 0, 10, 10, 500, 100 ])

    scene.add(polyline)

    renderer.renderDOMScene(scene)
}