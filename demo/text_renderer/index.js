
function play() {
    textRenderer = new Grapheme.TextRenderer()
    texts = [
        {
            style: {
                font: "Arial",
                fontSize: 100,
                shadowRadius: 0
            },
            text: "Text",
            rect: {}
        }
    ]

    textRenderer.drawText(texts)
    console.log(texts)

    document.body.appendChild(textRenderer.canvas)
}