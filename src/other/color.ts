import { leftZeroPad, staticImplements } from "../utils.js";
import { CompositionType } from "./composition_type.js";

export type ColorLike = Color | string | { r: number, g: number, b: number, a?: number }

function _clampRGB (x: number): number {
  return (((x < 0) ? 0 : ((x > 255) ? 255 : x)) + 0.5) | 0
}

function throwBadColor (s: string): never {
  throw new Error('Unrecognized colour ' + s)
}

export class Color {
  // Range: 0–255, inclusive
  r: number;
  g: number;
  b: number;
  a: number;

  constructor (r: number, g: number, b: number, a: number) {
    this.r = r
    this.g = g
    this.b = b
    this.a = a
  }

  /**
   * Convert RGB to color, clamping the values to the appropriate range. Each component should be in the range [0,255].
   * @param r Red component
   * @param g Green component
   * @param b Blue component
   */
  static rgb (r: number, g: number, b: number) {
    return new Color(_clampRGB(r), _clampRGB(g), _clampRGB(b), 255)
  }

  /**
   * Convert RGBA to color, clamping the values to the appropriate range. Each component should be in the range [0,255].
   * @param r Red component
   * @param g Green component
   * @param b Blue component
   * @param a Opacity component
   */
  static rgba (r: number, g: number, b: number, a = 255) {
    return new Color(_clampRGB(r), _clampRGB(g), _clampRGB(b), _clampRGB(a))
  }

  /**
   * Convert HSL to Color. The hue should be in radians, the saturation in [0,1], and the luminance in [0,1].
   * @param h Hue
   * @param s Saturation
   * @param l Luminance
   */
  static hsl (h: number, s: number, l: number) {
    let [ r, g, b ] = hslToRgb(h * (1 / (2 * Math.PI)), s * (1 / 100), l * (1 / 100))

    return Color.rgb(r, g, b)
  }

  /**
   * Convert HSLA to Color. The hue should be in radians, the saturation in [0,1], the luminance in [0,1], and the
   * opacity in [0,255].
   * @param h Hue
   * @param s Saturation
   * @param l Luminance
   * @param a Opacity
   */
  static hsla (h: number, s: number, l: number, a: number): Color {
    let color = Color.hsl(h, s, l)
    color.a = _clampRGB(a)

    return color
  }

  /**
   * Convert from hex string ("#231", "#00ff22" or "#00ff22aa") to Color.
   * @param s Hex string
   */
  static fromHex (s: string) {
    return hexToRgb(s)
  }

  /**
   * Convert from a (subset) of CSS color strings to a Color. Supported formats are hsla(degrees, percent, percent,
   * [0,1]), hsl(...), rgb(...), rgba(...), named colors like "blue", and hex strings. An error is thrown on an unknown
   * color.
   * @param s CSS string
   */
  static fromCss (s: string): Color {
    s = s.toLowerCase().replace(/\s+/g, '')
    if (s.startsWith('#')) {
      return Color.fromHex(s)
    }

    let argsMatch = /\((.+)\)/g.exec(s)

    if (!argsMatch) {
      let color = Colors[s.toUpperCase()]

      return color ? color : throwBadColor(s)
    }

    let args = argsMatch[1].split(',').map(Number.parseFloat)

    if (s.startsWith('rgb')) {
      return Color.rgba(args[0], args[1], args[2], s.startsWith('rgba') ? (_clampRGB(args[3] * 255)) : 255)
    } else if (s.startsWith('hsl')) {
      // args[1] and args[2] are percentages and so must be scaled. args[0], the luminance, is in [0, 255]
      args[0] *= Math.PI / 180
      if (s[3] === 'a') {
        return Color.hsla(args[0], args[1], args[2], args[3] * 255)
      }

      return Color.hsl(args[0], args[1], args[2])
    }

    throwBadColor(s)
  }

  static compose(...args: ColorLike[]): Color {
    if (args.length === 0) {
      return Color.default()
    }

    // Use the last specification
    return Color.fromObj(args[args.length - 1])
  }

  static create(spec: ColorLike): Color {
    return Color.fromObj(spec)
  }

  /**
   * Permissively convert an object to a color; returns black if conversion failed
   */
  static fromObj (obj: unknown): Color {
    if (typeof obj === 'string') {
      return Color.fromCss(obj)
    }

    // @ts-ignore
    if (obj && typeof obj.r === 'number' && typeof obj.g === 'number' && typeof obj.b === 'number') {
      // @ts-ignore
      return Color.rgba(obj.r, obj.g, obj.b, obj.a ?? 255)
    }

    return Color.default()
  }

  static default(): Color {
    return Color.rgba(0, 0, 0, 255)
  }

  rounded () {
    return {
      r: (this.r + 0.5) | 0,
      g: (this.g + 0.5) | 0,
      b: (this.b + 0.5) | 0,
      a: (this.a + 0.5) | 0
    }
  }

  /**
   * Convert color to a CSS-style hex string
   */
  hex (): string {
    const rnd = this.rounded()
    return `#${[rnd.r, rnd.g, rnd.b, rnd.a]
      .map(x => leftZeroPad(x.toString(16), 2))
      .join('')}`;
  }

  /**
   * Convert this color into a packed hex number (including an opacity value)
   */
  toNumber (): number {
    return this.r * 0x1000000 + this.g * 0x10000 + this.b * 0x100 + this.a
  }

  clone () {
    return new Color(this.r, this.g, this.b, this.a)
  }

  equals (c: Color) {
    return c.r === this.r && c.g === this.g && c.b === this.b && c.a === this.a
  }
}

function hexToRgb (hex: string): Color {
  hex = hex.replace(/#/g, '').trim()
  let r = 0, g = 0, b = 0, a = 255

  if (hex.length === 3 || hex.length === 4) {
    r = Number.parseInt(hex[0], 16)
    g = Number.parseInt(hex[1], 16)
    b = Number.parseInt(hex[2], 16)
    if (hex.length === 4) {
      a = Number.parseInt(hex[3], 16)
    }
  } else if (hex.length === 6 || hex.length === 8) {
    r = Number.parseInt(hex.slice(0, 2), 16)
    g = Number.parseInt(hex.slice(2, 4), 16)
    b = Number.parseInt(hex.slice(4, 6), 16)

    if (hex.length === 8) {
      a = Number.parseInt(hex.slice(6, 8), 16)
    }
  } else {
    throwBadColor(hex)
  }

  return Color.rgba(r, g, b, a)
}

// Credit to https://stackoverflow.com/a/9493060/13458117
function hue2Rgb (p: number, q: number, t: number): number {
  if (t < 0) t += 1
  if (t > 1) t -= 1
  if (t < 1 / 6) return p + (q - p) * 6 * t
  if (t < 1 / 2) return q
  if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6
  return p
}

function hslToRgb (h: number, s: number, l: number): [ number, number, number ] {
  // h, s, l in [0,1]
  let r, g, b

  if (s === 0) {
    r = g = b = l // achromatic
  } else {
    var q = l < 0.5 ? l * (1 + s) : l + s - l * s
    var p = 2 * l - q
    r = hue2Rgb(p, q, h + 1 / 3)
    g = hue2Rgb(p, q, h)
    b = hue2Rgb(p, q, h - 1 / 3)
  }

  return [ 255 * r, 255 * g, 255 * b ]
}

const rgb = Color.rgb

export const Colors = Object.freeze({
  get LIGHTSALMON () {
    return rgb(255, 160, 122)
  },
  get SALMON () {
    return rgb(250, 128, 114)
  },
  get DARKSALMON () {
    return rgb(233, 150, 122)
  },
  get LIGHTCORAL () {
    return rgb(240, 128, 128)
  },
  get INDIANRED () {
    return rgb(205, 92, 92)
  },
  get CRIMSON () {
    return rgb(220, 20, 60)
  },
  get FIREBRICK () {
    return rgb(178, 34, 34)
  },
  get RED () {
    return rgb(255, 0, 0)
  },
  get DARKRED () {
    return rgb(139, 0, 0)
  },
  get CORAL () {
    return rgb(255, 127, 80)
  },
  get TOMATO () {
    return rgb(255, 99, 71)
  },
  get ORANGERED () {
    return rgb(255, 69, 0)
  },
  get GOLD () {
    return rgb(255, 215, 0)
  },
  get ORANGE () {
    return rgb(255, 165, 0)
  },
  get DARKORANGE () {
    return rgb(255, 140, 0)
  },
  get LIGHTYELLOW () {
    return rgb(255, 255, 224)
  },
  get LEMONCHIFFON () {
    return rgb(255, 250, 205)
  },
  get LIGHTGOLDENRODYELLOW () {
    return rgb(250, 250, 210)
  },
  get PAPAYAWHIP () {
    return rgb(255, 239, 213)
  },
  get MOCCASIN () {
    return rgb(255, 228, 181)
  },
  get PEACHPUFF () {
    return rgb(255, 218, 185)
  },
  get PALEGOLDENROD () {
    return rgb(238, 232, 170)
  },
  get KHAKI () {
    return rgb(240, 230, 140)
  },
  get DARKKHAKI () {
    return rgb(189, 183, 107)
  },
  get YELLOW () {
    return rgb(255, 255, 0)
  },
  get LAWNGREEN () {
    return rgb(124, 252, 0)
  },
  get CHARTREUSE () {
    return rgb(127, 255, 0)
  },
  get LIMEGREEN () {
    return rgb(50, 205, 50)
  },
  get LIME () {
    return rgb(0, 255, 0)
  },
  get FORESTGREEN () {
    return rgb(34, 139, 34)
  },
  get GREEN () {
    return rgb(0, 128, 0)
  },
  get DARKGREEN () {
    return rgb(0, 100, 0)
  },
  get GREENYELLOW () {
    return rgb(173, 255, 47)
  },
  get YELLOWGREEN () {
    return rgb(154, 205, 50)
  },
  get SPRINGGREEN () {
    return rgb(0, 255, 127)
  },
  get MEDIUMSPRINGGREEN () {
    return rgb(0, 250, 154)
  },
  get LIGHTGREEN () {
    return rgb(144, 238, 144)
  },
  get PALEGREEN () {
    return rgb(152, 251, 152)
  },
  get DARKSEAGREEN () {
    return rgb(143, 188, 143)
  },
  get MEDIUMSEAGREEN () {
    return rgb(60, 179, 113)
  },
  get SEAGREEN () {
    return rgb(46, 139, 87)
  },
  get OLIVE () {
    return rgb(128, 128, 0)
  },
  get DARKOLIVEGREEN () {
    return rgb(85, 107, 47)
  },
  get OLIVEDRAB () {
    return rgb(107, 142, 35)
  },
  get LIGHTCYAN () {
    return rgb(224, 255, 255)
  },
  get CYAN () {
    return rgb(0, 255, 255)
  },
  get AQUA () {
    return rgb(0, 255, 255)
  },
  get AQUAMARINE () {
    return rgb(127, 255, 212)
  },
  get MEDIUMAQUAMARINE () {
    return rgb(102, 205, 170)
  },
  get PALETURQUOISE () {
    return rgb(175, 238, 238)
  },
  get TURQUOISE () {
    return rgb(64, 224, 208)
  },
  get MEDIUMTURQUOISE () {
    return rgb(72, 209, 204)
  },
  get DARKTURQUOISE () {
    return rgb(0, 206, 209)
  },
  get LIGHTSEAGREEN () {
    return rgb(32, 178, 170)
  },
  get CADETBLUE () {
    return rgb(95, 158, 160)
  },
  get DARKCYAN () {
    return rgb(0, 139, 139)
  },
  get TEAL () {
    return rgb(0, 128, 128)
  },
  get POWDERBLUE () {
    return rgb(176, 224, 230)
  },
  get LIGHTBLUE () {
    return rgb(173, 216, 230)
  },
  get LIGHTSKYBLUE () {
    return rgb(135, 206, 250)
  },
  get SKYBLUE () {
    return rgb(135, 206, 235)
  },
  get DEEPSKYBLUE () {
    return rgb(0, 191, 255)
  },
  get LIGHTSTEELBLUE () {
    return rgb(176, 196, 222)
  },
  get DODGERBLUE () {
    return rgb(30, 144, 255)
  },
  get CORNFLOWERBLUE () {
    return rgb(100, 149, 237)
  },
  get STEELBLUE () {
    return rgb(70, 130, 180)
  },
  get ROYALBLUE () {
    return rgb(65, 105, 225)
  },
  get BLUE () {
    return rgb(0, 0, 255)
  },
  get MEDIUMBLUE () {
    return rgb(0, 0, 205)
  },
  get DARKBLUE () {
    return rgb(0, 0, 139)
  },
  get NAVY () {
    return rgb(0, 0, 128)
  },
  get MIDNIGHTBLUE () {
    return rgb(25, 25, 112)
  },
  get MEDIUMSLATEBLUE () {
    return rgb(123, 104, 238)
  },
  get SLATEBLUE () {
    return rgb(106, 90, 205)
  },
  get DARKSLATEBLUE () {
    return rgb(72, 61, 139)
  },
  get LAVENDER () {
    return rgb(230, 230, 250)
  },
  get THISTLE () {
    return rgb(216, 191, 216)
  },
  get PLUM () {
    return rgb(221, 160, 221)
  },
  get VIOLET () {
    return rgb(238, 130, 238)
  },
  get ORCHID () {
    return rgb(218, 112, 214)
  },
  get FUCHSIA () {
    return rgb(255, 0, 255)
  },
  get MAGENTA () {
    return rgb(255, 0, 255)
  },
  get MEDIUMORCHID () {
    return rgb(186, 85, 211)
  },
  get MEDIUMPURPLE () {
    return rgb(147, 112, 219)
  },
  get BLUEVIOLET () {
    return rgb(138, 43, 226)
  },
  get DARKVIOLET () {
    return rgb(148, 0, 211)
  },
  get DARKORCHID () {
    return rgb(153, 50, 204)
  },
  get DARKMAGENTA () {
    return rgb(139, 0, 139)
  },
  get PURPLE () {
    return rgb(128, 0, 128)
  },
  get INDIGO () {
    return rgb(75, 0, 130)
  },
  get PINK () {
    return rgb(255, 192, 203)
  },
  get LIGHTPINK () {
    return rgb(255, 182, 193)
  },
  get HOTPINK () {
    return rgb(255, 105, 180)
  },
  get DEEPPINK () {
    return rgb(255, 20, 147)
  },
  get PALEVIOLETRED () {
    return rgb(219, 112, 147)
  },
  get MEDIUMVIOLETRED () {
    return rgb(199, 21, 133)
  },
  get WHITE () {
    return rgb(255, 255, 255)
  },
  get SNOW () {
    return rgb(255, 250, 250)
  },
  get HONEYDEW () {
    return rgb(240, 255, 240)
  },
  get MINTCREAM () {
    return rgb(245, 255, 250)
  },
  get AZURE () {
    return rgb(240, 255, 255)
  },
  get ALICEBLUE () {
    return rgb(240, 248, 255)
  },
  get GHOSTWHITE () {
    return rgb(248, 248, 255)
  },
  get WHITESMOKE () {
    return rgb(245, 245, 245)
  },
  get SEASHELL () {
    return rgb(255, 245, 238)
  },
  get BEIGE () {
    return rgb(245, 245, 220)
  },
  get OLDLACE () {
    return rgb(253, 245, 230)
  },
  get FLORALWHITE () {
    return rgb(255, 250, 240)
  },
  get IVORY () {
    return rgb(255, 255, 240)
  },
  get ANTIQUEWHITE () {
    return rgb(250, 235, 215)
  },
  get LINEN () {
    return rgb(250, 240, 230)
  },
  get LAVENDERBLUSH () {
    return rgb(255, 240, 245)
  },
  get MISTYROSE () {
    return rgb(255, 228, 225)
  },
  get GAINSBORO () {
    return rgb(220, 220, 220)
  },
  get LIGHTGRAY () {
    return rgb(211, 211, 211)
  },
  get SILVER () {
    return rgb(192, 192, 192)
  },
  get DARKGRAY () {
    return rgb(169, 169, 169)
  },
  get GRAY () {
    return rgb(128, 128, 128)
  },
  get DIMGRAY () {
    return rgb(105, 105, 105)
  },
  get LIGHTSLATEGRAY () {
    return rgb(119, 136, 153)
  },
  get SLATEGRAY () {
    return rgb(112, 128, 144)
  },
  get DARKSLATEGRAY () {
    return rgb(47, 79, 79)
  },
  get BLACK () {
    return rgb(0, 0, 0)
  },
  get CORNSILK () {
    return rgb(255, 248, 220)
  },
  get BLANCHEDALMOND () {
    return rgb(255, 235, 205)
  },
  get BISQUE () {
    return rgb(255, 228, 196)
  },
  get NAVAJOWHITE () {
    return rgb(255, 222, 173)
  },
  get WHEAT () {
    return rgb(245, 222, 179)
  },
  get BURLYWOOD () {
    return rgb(222, 184, 135)
  },
  get TAN () {
    return rgb(210, 180, 140)
  },
  get ROSYBROWN () {
    return rgb(188, 143, 143)
  },
  get SANDYBROWN () {
    return rgb(244, 164, 96)
  },
  get GOLDENROD () {
    return rgb(218, 165, 32)
  },
  get PERU () {
    return rgb(205, 133, 63)
  },
  get CHOCOLATE () {
    return rgb(210, 105, 30)
  },
  get SADDLEBROWN () {
    return rgb(139, 69, 19)
  },
  get SIENNA () {
    return rgb(160, 82, 45)
  },
  get BROWN () {
    return rgb(165, 42, 42)
  },
  get MAROON () {
    return rgb(128, 0, 0)
  },
  get TRANSPARENT () {
    return new Color(0, 0, 0, 0)
  }
} as const)

staticImplements<CompositionType<Color, ColorLike>>(Color)
