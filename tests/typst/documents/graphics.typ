#set page(width: 160mm, height: 210mm, margin: 14mm)
= Vector Graphics
#rect(width: 65mm, height: 20mm, fill: gradient.linear(red, blue), radius: 5pt)
#v(8pt)
#circle(radius: 12mm, fill: yellow, stroke: 2pt + black)
#ellipse(width: 40mm, height: 16mm, fill: aqua, stroke: 1pt + blue)
#line(length: 65mm, angle: 15deg, stroke: 2pt + green)
#rotate(12deg, reflow: true)[*Rotated text*]
#scale(x: 120%, y: 80%, reflow: true)[Scaled text]
#image("diagram.svg", width: 65mm)
