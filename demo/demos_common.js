let displayError = s => {
  DemoContainer.innerHTML = `<div class="major-error">${escapeHtml(s)}</div>`
}

function escapeHtml(unsafe) {
  return unsafe
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#039;")
}

function setCodeOutput (s, isError=false) {
  let o = document.getElementById("main-code-output")
  if (!o) return

  o.innerHTML = escapeHtml(s)
  if (isError) {
    o.classList.add("major-error")
  } else {
    o.classList.remove("major-error")
  }
}

function errToString (err) {
  return (err.stack ?? err.toString()) + ''
}

let DemoContainer = document.getElementById("demo-container")
if (!DemoContainer) {
  displayError = () => {}
}

// Get logo as dom element
let logo = document.createElement("img")
logo.src = "../logo.svg"
logo.onclick = () => {
  console.log ("hi")
  document.location.href = "https://github.com/anematode/grapheme-math"
}

logo.classList.add("logo")

document.getElementById("header").appendChild(logo)

{
  // Wait for Grapheme to load, then call play()
  let intervalHandler = setInterval(() => {
    let Grapheme = window.Grapheme

    if (Grapheme) {
      // Grapheme loaded
      if (window.play) {
        try {
          window.play()
        } catch (e) {
          displayError(errToString(e))  // try to make it look nice
        }
      } else {
        displayError("window.play is not a function")
      }

      clearHandler()
    }
  }, 200)
  
  function clearHandler () {
    clearInterval(intervalHandler)
  }

  setTimeout(() => {
    if (!window.Grapheme) {
      displayError("Grapheme failed to load")

      clearHandler()
    }
  }, 3000)
}
