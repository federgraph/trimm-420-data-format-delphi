# Reports

When working with the data it helps to use the textual reports.

## Trimm-Item Json

This is what a Trimm-Item looks like in json format:
```json
{"Name":"T0","Faktor":1,"OffsetX":0,"OffsetZ":0,
"RK":{
"A0":{"x":2560,"y":765,"z":430},
"C0":{"x":4140,"y":0,"z":340},
"D0":{"x":2870,"y":0,"z":-100},
"E0":{"x":2970,"y":0,"z":450},
"F0":{"x":-30,"y":0,"z":300}},
"RL":{"MU":2600,"MO":2000,"ML":6115,"MV":5000,"CA":50},
"SB":{
"CP":{"Min":50,"Pos":100,"Max":200},
"VO":{"Min":4400,"Pos":4500,"Max":4600},
"WI":{"Min":85,"Pos":95,"Max":105},
"WL":{"Min":4050,"Pos":4120,"Max":4200},
"WO":{"Min":2000,"Pos":2020,"Max":2070},
"SH":{"Min":140,"Pos":220,"Max":300},
"SA":{"Min":780,"Pos":850,"Max":1000},
"SL":{"Min":450,"Pos":479,"Max":600}}}
```

Not sure if I should format this properly or leave it as is.

```
RK = Rumpf-Koordinaten
RL = think Rigg-Längen or Mast-Längen ( ML already used for overall Mast-Length ) 
SB = Scroll-Bars
```

SB: I happend to use scroll bars in 1995. It was the best thing available. We got new Trackbar controls later.
Today we have sliders. But all these things can be used to change the value of the current parameter.

The old scroll bar control used Integer values.
The track bar and slider use single precision floating point values.
But the serialized data still uses Integer values.

When you set up a modern slider you should perhaps allow input values from -1000 to +1000
and then map the value to the actual range of the currently selected parameter.
It has the advantage that it will be easier if you use only one or even none of these controls to change the value of the current parameter.
Sometimes it is enough to swipe across the screen in a certain way,
you do not need a control any more these days.

## Trimm-Item Report

This is about the same data but formatted for display on screen.
```
A0 (2560, 765, 430)
C0 (4140, 0, 340)
D0 (2870, 0, -100)
E0 (2970, 0, 450)
F0 (-30, 0, 300)

ML, MV (6115, 5000)
MU, MO (2600, 2000)
CA (50)
h0, h2, l2 (56, 0, 100)

CP (50, 100, 200)
VO (4400, 4500, 4600)
WI (85, 95, 105)
WL (4050, 4120, 4200)
WO (2000, 2020, 2070)
SH (140, 220, 300)
SA (780, 850, 1000)
SL (450, 479, 600)
```
You can see it on the screen of the real app.
It is visible on top of the graph when `data` is switched on by the user.
There is not much space available to show text.
The Text goes into a TText control.

## Log Report

I have added an option in the published app
so that the user can show the log text instead of the Trimm-Item report,
in the same TText component.

Debugging the app on a device or inside of the simulator often does not work at all.
That is why it is valuable to have some old style *in app logging* capability built into the app.

> InApp logging is better for the user than InApp purchasing.
