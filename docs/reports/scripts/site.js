// Quarto build 
document.addEventListener("DOMContentLoaded", () => {
  // Update copyright year in the footer
  const yearEl = document.getElementById("copyright-year");
  if (yearEl) {
    yearEl.textContent = new Date().getFullYear();
  }

  // Smooth scroll for the back-to-top button
  const backToTop = document.getElementById("quarto-back-to-top");
  if (backToTop) {
    backToTop.addEventListener("click", (e) => {
      e.preventDefault();
      window.scrollTo({ top: 0, behavior: "smooth" });
    });
  }

  // Pair each existing light PNG with its matching -dark PNG. The document
  // theme (managed by Quarto) controls which sibling is visible via CSS.
  const pairFigures = () => {
    document.querySelectorAll("img[src]").forEach((lightImage) => {
      if (
        lightImage.closest(".tico-figure-pair") ||
        !/\/figures\/[^/]+\.png(?:$|\?)/.test(lightImage.getAttribute("src"))
      ) {
        return;
      }

      const lightSrc = lightImage.getAttribute("src");
      const darkSrc = lightSrc.replace(/\.png(?=$|\?)/, "-dark.png");
      if (darkSrc === lightSrc) return;

      const darkImage = lightImage.cloneNode(true);
      darkImage.src = darkSrc;
      darkImage.classList.remove("tico-light-figure");
      darkImage.classList.add("tico-dark-figure");
      lightImage.classList.add("tico-light-figure");

      const pair = document.createElement("span");
      pair.className = "tico-figure-pair";
      pair.setAttribute("aria-live", "polite");
      lightImage.before(pair);
      pair.append(lightImage, darkImage);
    });
  };

  pairFigures();

  const observer = new MutationObserver((mutations) => {
    if (mutations.some((mutation) => mutation.addedNodes.length)) {
      pairFigures();
    }
  });
  observer.observe(document.body, { childList: true, subtree: true });
});