// Double double arithmetic (taken from https://github.com/taschini/pycrlibm/blob/master/crlibm/double-extended.h)

export function addDD(resH, resL, xH, xL, yH, yL) {
  let r = xH + yH
  let t = r - xH
  let e = (xH - (r - t)) + (yH - t) + xL + yL

  resH = r + e
  resL = e - (resH - r)

  return [resH, resL]
}

export function subDD(resH, resL, xH, xL, yH, yL) {
  let r = xH - yH
  let t = r - xH
  let e = (xH - (r - t)) - (yH + t) + xL - yL

  resH = r + e
  resL = e - (resH - r)

  return [resH, resL]
}

export function mulDD(resH, resL, xH, xL, yH, yL) {
  // fma emulation for xH * yH
  let xHs = xH * 4294967297
  let yHs = yH * 4294967297

  // Corrected
  let xC = (xH - xHs) + xHs
  let yC = (yH - yHs) + yHs

  let xE = xH - xC
  let yE = yH - yC

  let prod = xH * yH
  let err = (xC * yC - prod) + xC * yE + yC * xE + xE * yE  // fancy correction factor, effectively a 2x2 multiplication

  err += xH * yL + yH * xL

  resH = prod + err
  resL = err - (resH - prod)

  return [resH, resL]
}

export function divDD(resH, resL, xH, xL, yH, yL) {
  let cH = xH / yH

  // fma emulation xH / yH  * yH
  let cHs = cH * 4294967297
  let yHs = yH * 4294967297

  let cC = (cH - cHs) + cHs
  let yC = (yH - yHs) + yHs

  let cE = cH - cC
  let yE = yH - yC

  let mH = cH * yH
  let mL = (cC * yC - mH) + cC * yE + yC * cE + cE * yE

  let cL = xH - mH - mL + xL - cH * yL
  cL /= yH

  resH = cH + cL
  resL = cL - (resH - cH)

  return [resH, resL]
}

// g = 14, k = 18

const ddLanczosCoefficients = [
  1.0004560836571694,-1.0988823033209335e-16,1069183.9982044045,-9.152943190896582e-11,-4731030.758059078,-4.467886426666599e-10,8830775.183233108,-1.974348155010428e-10,-9050031.081126565,7.362086423033288e-10,5449549.646453332,2.4619990211679536e-10,-820910.9635465388,7.0671362981615515e-12,-8397637.157595333,-8.283110976555393e-10,42610828.21614842,-2.2865463761596797e-09,-147425386.5000112,-2.4331015609118278e-09,372680553.24447244,1.4813200858822748e-08,-694440432.3598851,-2.9109135508174643e-09,951844496.766441,1.3734026444876301e-08,-947268303.1678969,2.383581191737024e-08,665426591.6707596,-4.7452663988393134e-08,-312635200.51296365,1.118584387560512e-08,88121096.82010905,5.364721724681045e-10,-11264045.220948199,4.838749869371055e-10
]

const ddCoeffLength = 18

// TODO
function gammaRealHighPrec(x) {
  x = +x

  x -= 1 // always exact
  let sH = ddLanczosCoefficients[0], sL = ddLanczosCoefficients[1]

  for (let i = 0; i < ddCoeffLength; ++i) {
    let coeffH = ddLanczosCoefficients[2 * i + 2], coeffL = ddLanczosCoefficients[2 * i + 3]

    let dH = x + i + 1, dL = sL

    // Division coeff / d
    let cH = coeffH / dH

    // fma emulation xH / yH  * yH
    let cHs = cH * 4294967297
    let yHs = dL * 4294967297

    let cC = (cH - cHs) + cHs
    let yC = (dL - yHs) + yHs

    let cE = cH - cC
    let yE = dL - yC

    let mH = cH * dL
    let mL = (cC * yC - mH) + cC * yE + yC * cE + cE * yE

    let cL = dH - mH - mL + coeffL - cH * dL
    cL /= dL

    let addendH = cH + cL
    let addendL = cL - (addendH - cH)

    let r = sH + addendH
    let t = r - sH
    let e = (sH - (r - t)) + (addendH - t) + sL + addendL

    sH = r + e
    sL = e - (sH - r)
  }
}

