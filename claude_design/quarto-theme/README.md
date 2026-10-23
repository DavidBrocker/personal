# Organic Quarto theme

Drop-in replacement for `custom.scss` in your `personal` repo — same filename, same `_quarto.yml` reference (`theme: [cosmo, custom.scss]`), no other config changes needed.

## What changed
- Palette: old kurbits blue/red/gold → plum (`#7d3a55`) + sage (`#7a8a5e`) on cream (`#f5ead8`), matching the redesigned site.
- Fonts: dropped the Google Fonts `@import` (Noto Sans Display) in favor of the system font stack — one less network dependency, matches the rest of the site.
- Every `*-card` class (`.app-card`, `.class-taught`, `.writing-card`, `.gallery-card`, `.tutorial-card`) lost its left accent border in favor of a plain rounded card + hover-lift — variety now comes from spacing/weight, not new colors, per your call.
- Removed the 🎓 emoji overlay on `.class-taught`.
- Callouts (`note`/`important`/`tip`) now map to sage / plum / a warm neutral — still 3 distinct signals without a 3rd hue.
- Footer restyled to dark plum + cream, echoing the Teaching page's review marquee.
- webR run button, live editor, and output panes recolored to match (`.btn-webr`, `[id^="webr-editor-"]`, `[id^="webr-code-output-"]`).
- Kept `.title`, `.about`, `.five`, and `.hex` (all found in real use — homepage bio block, vis1.qmd's demo box) — recolored/simplified rather than dropped.

## Follow-up (not done here)
`kurbits-corner.svg`, `kurbits-divider.svg`, and `kurbits-rosette.svg` still bake the old blue/gold in as fill colors inside the SVG files themselves. The scss references them unchanged. Recolor those three files to plum/sage to finish the match — happy to do that next if you want them updated too.
