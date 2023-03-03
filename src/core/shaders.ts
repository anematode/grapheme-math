export const MonochromaticGeometryProgram = {
    vertex: `
precision highp float;
attribute vec2 vertexPosition;
// Transforms a vertex from pixel coordinates to clip space
uniform vec2 xyScale;
vec2 displacement = vec2(-1, 1);
         
void main() {
   gl_Position = vec4(vertexPosition * xyScale + displacement, 0, 1);
}`,
    fragment: `precision highp float;
uniform vec4 color;
        
void main() {
   gl_FragColor = color;
}`
}