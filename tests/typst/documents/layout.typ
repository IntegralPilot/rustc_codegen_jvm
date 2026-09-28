#set page(width: 160mm, height: 210mm, margin: 14mm)
#set text(size: 10pt)
#set heading(numbering: "1.1")
#set par(justify: true)
= Compiler Report
This paragraph contains *bold text*, _emphasis_, `inline code`, and a
#link("https://typst.app")[link]. Typography: office, affine, résumé, naïve,
Grüße, 1234567890.

== Structure
- A short item
- A second item with *styled content*
  - A nested item
+ First ordered item
+ Second ordered item

#block(inset: 8pt, fill: rgb("edf3ff"), stroke: 0.5pt + blue, radius: 4pt)[
  A coloured block with a footnote#footnote[Footnote text verifies placement.]
  and enough text to exercise line wrapping across its width.
]
#columns(2, gutter: 12pt)[
  #for i in range(1, 7) [
    *Section #i.* Layout handles text wrapping, justification, and paragraph
    spacing across columns. The quick brown fox jumps over the lazy dog.

  ]
]
