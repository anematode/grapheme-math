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

let DemoContainer = document.getElementById("demo-container")
if (!DemoContainer) {
  displayError = () => {}
}

// Get logo as dom element
let logo = document.createElement("img")
logo.src = "../logo.svg"
logo.classList.add("logo")

document.getElementById("header").appendChild(logo)

{
  function clearHandler () {
    clearInterval(intervalHandler)
  }

  // Wait for Grapheme to load, then call play()
  let intervalHandler = setInterval(() => {
    let Grapheme = window.Grapheme

    if (Grapheme) {
      // Grapheme loaded
      if (window.play) {
        try {
          window.play()
        } catch (e) {
          displayError((e.stack ?? e.toString()) + '')  // try to make it look nice
        }
      } else {
        displayError("window.play is not a function")
      }

      clearHandler()
    }
  }, 200)

  setTimeout(() => {
    if (!window.Grapheme) {
      displayError("Grapheme failed to load")

      clearHandler()
    }
  }, 3000)
}
