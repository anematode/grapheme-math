function play() {
    renderer = new Grapheme.WebGLRenderer()
    scene = new Grapheme.InteractiveScene()

    document.getElementById('container').appendChild(scene.domElement)
    plot2d = new Grapheme.Plot2D()

    polyline = new Grapheme.PolylineElement()

    plot2d.add(polyline)
    polyline.setVertices([ 0, 0, 200, 200, 10, 0, 210, 200 ])

    scene.add(plot2d)
    scene.updateAll()

    plot2d.fitScene()

    renderer.renderDOMScene(scene)
}