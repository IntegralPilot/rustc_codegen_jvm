#set page(width: 180mm, height: 230mm, margin: 12mm)
= Table and Grid
#table(
  columns: (1fr, 2fr, 1fr),
  inset: 7pt,
  fill: (x, y) => if calc.odd(y) { rgb("edf3ff") },
  table.header([*Item*], [*Description*], [*Value*]),
  [Alpha], [A wrapping description with *bold* and _italic_ text], [12.50],
  [Beta], [Second item], [23.75],
  table.cell(colspan: 2)[*Total*], [36.25],
)
#v(12pt)
#grid(columns: (1fr, 1fr), gutter: 8pt,
  block(fill: aqua.lighten(75%), inset: 8pt)[Top left],
  block(fill: yellow.lighten(75%), inset: 8pt)[Top right],
  block(fill: green.lighten(75%), inset: 8pt)[Bottom left],
  block(fill: red.lighten(75%), inset: 8pt)[Bottom right],
)
