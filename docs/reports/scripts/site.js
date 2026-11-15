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
});