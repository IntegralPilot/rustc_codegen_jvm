#set page(width: 160mm, height: 210mm, margin: 14mm, numbering: "1")
#set heading(numbering: "1.1")
= Introduction <intro>
#outline()
See @results for the results and @diagram for a figure.
#figure(rect(width: 45mm, height: 20mm, fill: blue.lighten(80%)), caption: [A coloured diagram.]) <diagram>
#pagebreak()
= Results <results>
Return to @intro. This page contains a counter: #context counter(heading).display().
#context [There are #query(heading).len() headings.]
#pagebreak()
= Conclusion
Three pages verify repeated headers, references, and layout introspection.
