#import "helpers.typ": card
#set page(width: 160mm, height: 210mm, margin: 14mm)
= Data and Functions
#let values = json("values.json")
#assert.eq(values.items.map(it => it.count).sum(), 42)
#for item in values.items [
  #card(item.name, item.count)
]
#let matches = "alpha-12 beta-30".matches(regex("[0-9]+"))
#assert.eq(matches.map(it => int(it.text)).sum(), 42)
The total is #values.items.map(it => it.count).sum().
#table(columns: 2, ..csv("values.csv").flatten())
