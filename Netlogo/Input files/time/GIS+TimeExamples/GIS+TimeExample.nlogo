extensions [ gis table csv time ]

breed [ cells cell ]

globals
[
  ; Input file names
  polygon-file-name
  time-series-file-name

  ; Display and output parameters
  world-resolution     ; Display resolution: meters per patch
  write-frames?        ; If true, writes View to a graphics file each tick, for making an animation
  file-output-frequency ; Number of time units between file output updates - must be an integer
  file-output-units    ; Time units for file-output-frequency: "hours", "days", "weeks", "months", or "years"

  ; Time step variables
  time                   ; Logo-time variable for current time
  formatted-time         ; String variable for current time
  end-time-code          ; Logo-time variable for ending time
  next-time              ; Logo-time variable for when the current time step ends
  next-output-time       ; Logo-time variable when file output is next written
  step-length            ; Duration of the current time step (days)
  prev-step-length       ; Duration of previous time step (days)

  ; Global variables
  julian-date            ; Julian date of current time (start of time step)
  flow                   ; Current river flow (m3/s)
  temperature            ; Current water temperature (C)
  turbidity              ; Current river turbidity (NTU)
  run-time               ; Model execution time (s)

  ; Internal data structures
  polygons-dataset    ; The cell polygons, from GIS input
  cell-ID-table       ; A *table* linking cell numbers to corresponding cells
  model-patches       ; An agentset of the patches that are within the modeled space
  non-model-patches   ; The patches that are not within modeled space
  input-time-series   ; Time-series input set (time extension's LogoTime data type)
  input-time-list     ; List of times in input-time-series

]

patches-own
[
  ; Static variables
  patches-cell            ; The cell that the patch belongs to
  patches-cells-number    ; The number of the cell that the patch is in.

]

cells-own        ; Cells are turtles that represent the polygonal habitat units
[
  ; Static state variables
  cell-number
  cell-dist-to-hide            ; Characteristic distance to hiding cover (input in m; converted to cm)
  cell-area                    ; Cell area (cm2); read in from shapefile
  cell-area-approx             ; Cell area (cm2), calculated from the number of patches, for testing only
  max-dist-to-adjacent-cell    ; Distance (cm) from cell to farthest member of adjacent-cells

  ; Agentsets
  cells-patches  ; The NetLogo patches inside the cell
  adjacent-cells ; The cells that represent adjacent (intersecting) polygons; these include self

]

to setup

  ; Using the following separate "clear" statements (but not "clear-globals") instead of
  ; clear-all allows BehaviorSpace to use global variables without putting them on the Interface.
  ; But this makes it critical to make sure all global variables are initialized in set-parameters,
  ; even if their initial value is zero.
  clear-ticks
  clear-turtles
  clear-patches
  clear-drawing
  clear-all-plots
  clear-output

  reset-ticks

  set-parameters

  build-cells

  set-times-and-inputs

end

to go

  if (ticks < 1) [ reset-timer ]

  tick  ; Advance the time step counter

  ; Determine length of the current time step: the time (days) between current time and
  ; the next time in the input file.
  ; Time values are all taken from the list of time values in the input file
  ; If it is not the first tick, advance the time by reading the first value on the time list
  set time time:copy first input-time-list
  set formatted-time time:show time "yyyy/MM/dd HH:mm"

  ; Then remove the current time from the list, and stop if it is the last time.
  set input-time-list but-first input-time-list
  if empty? input-time-list
  [
    set run-time timer
    show (word "Execution finished in " run-time " seconds")
    stop
  ]

  ; Now, the first item on the time list is the start of the *next* time step
  set next-time time:copy first input-time-list

  ; Calculate the time step length, and remember length of previous step
  set prev-step-length step-length
  set step-length time:difference-between time next-time "days"


  ; Set cell depths, velocities, light, food
  update-habitat         ; This procedure is on this tab, below

  ; Update outputs
  update-output

end


to update-habitat
  ; Observer procedure to set flow, temperature, depth, velocity, light, food of patches
  ; See the procedure test-hydraulics for a way to test depth and velocity updates

  ; First, get today's flow and temperature
  set flow time:ts-get input-time-series time "flow"
  set temperature time:ts-get input-time-series time "temperature"
  set turbidity time:ts-get input-time-series time "turbidity"

end

to set-parameters

  ; Setup files
  set polygon-file-name "Input-files/A-site.shp"             ; Cell geometry shapefile name
  set time-series-file-name "Input-files/A-site-2HrTimeSeriesInputs.csv"; Time-series inputs (flow & temperature)


  ; Display and output parameters
  set world-resolution     0.8     ; Patch size (m)
  set write-frames?      false     ; true to save view to a graphics file each tick
  set file-output-frequency  2     ; Number of time units between file output updates - must be an integer
  set file-output-units "hours"    ; Time units for file-output-frequency: "hours", "days", "weeks", "months", or "years"

end


to build-cells
  ; Observer procedure to read GIS data and build habitat cells

  ; Create the table linking cell numbers to cell turtles
  set cell-ID-table table:make

  ; Initialize patch cell numbers to identify those not in cells
  ask patches [ set patches-cells-number -99999 ]

  ; Read in the shapefile of cell polygons
  set polygons-dataset gis:load-dataset polygon-file-name
   show gis:property-names polygons-dataset

  ; Re-size the world according to the polygon range and display resolution
  let space-envelope gis:envelope-of polygons-dataset
  let gis-x-range ((item 1 space-envelope) - (item 0 space-envelope))
  let gis-y-range ((item 3 space-envelope) - (item 2 space-envelope))
  let display-x-range ceiling (gis-x-range) / world-resolution
  let display-y-range ceiling (gis-y-range) / world-resolution

  resize-world 0 display-x-range 0 display-y-range

  show (word "Reach extents: " precision gis-x-range 2 " m east-west, " precision gis-y-range 2 " m north-south")

  ; Convert GIS polygons to World space
  gis:set-world-envelope space-envelope

  gis:set-drawing-color white
  gis:draw polygons-dataset 1

  ; Copy polygon cell number into its patch variable
  gis:apply-coverage polygons-dataset "CELLID" patches-cells-number

  ; Cell numbers must be strings!
  set model-patches patches with [ is-string? patches-cells-number ]
  set non-model-patches patches with [ not is-string? patches-cells-number ]
  show (word "Number of patches in reach: " count model-patches)
  if any? model-patches with [is-number? patches-cells-number]
    [ error "Build-cells: some patches have non-string values of patches-cells-number" ]

  ; Each polygon creates a cell
  ; Cells set cells-patches to patches they contain
  foreach (gis:feature-list-of polygons-dataset )
  [ the-polygon ->
    create-cells 1
    [
      ; Initialize cell variables so we can test whether they were input
      set cell-number -99
      set cell-dist-to-hide -99

      ; Read in cell variables
      set cell-number gis:property-value the-polygon "CELLID"
      set cell-area (gis:property-value the-polygon "HABC_AREA") * 100 * 100    ; Convert area from m2 to cm2
      set cell-dist-to-hide (gis:property-value the-polygon "DISCOV") * 100     ; Characteristic distance to hiding cover (input in m; converted to cm)

      let cell-centroid gis:centroid-of the-polygon
      let centroid-location gis:location-of cell-centroid
      setxy item 0 centroid-location item 1 centroid-location
      set shape "circle"
      set size 2

      set cells-patches patches with [ patches-cells-number = [cell-number] of myself ]
      ask cells-patches [set patches-cell myself]

      ; APPROXIMATION FOR AREA - can be used to test shapefile values
      ; set cell-area-approx (count cells-patches) * world-resolution * world-resolution * 10000

      ; Check for cells that have no patches within them, which would cause big errors.
      if count cells-patches = 0
        [ error (word "Cell " cell-number " has no patches. Reduce the value of world-resolution.")]

      ; So cell can use patch variables, make sure it is in one of its patches
      ; (Does not always happen because centroid of irregular polygon may not be in polygon)
      if patches-cells-number != cell-number [ move-to min-one-of cells-patches [distance myself]]

      ; Put the cell in the cell-number lookup table
      table:put cell-ID-table cell-number self

    ]
  ]

  ; Build agentsets of adjacent cells by looping through the polygons
  ; and finding intersecting ones
  foreach gis:feature-list-of polygons-dataset
  [
    this-poly ->
    let polys-cell-number gis:property-value this-poly "CELLID"
    let polys-cell table:get cell-ID-table polys-cell-number

    ; initialize the agentset of adjacent cells
    ask polys-cell [set adjacent-cells no-turtles]

    foreach gis:feature-list-of polygons-dataset
    [
      another-poly ->
      if gis:intersects? this-poly another-poly
      [
        let adjacent-cell-number gis:property-value another-poly "CELLID"
        ask polys-cell
        [
          set adjacent-cells (turtle-set adjacent-cells (table:get cell-ID-table adjacent-cell-number))
        ]
      ]
    ]

  ]

  ; Now set the cell's variable for max distance to an adjacent cell and do some error checking
  ask cells
  [
    set max-dist-to-adjacent-cell (100 * world-resolution) * max [distance myself] of adjacent-cells
    if max-dist-to-adjacent-cell < 10
     [ error "In build-cells, furthest adjacent cell is less than 10 cm away" ]
    if patches-cells-number != cell-number
     [ error (word "Cell is not in of one of its patches, cell number " cell-number)]
    if is-number? cell-number
     [ error "Cell number not initialized or initialized to a number instead of string"]
    if (cell-dist-to-hide = -99)
     [ error (word "cell-dist-to-hide not initialized at cell number " cell-number)]
    if not is-number? cell-dist-to-hide
     [ error (word "cell-dist-to-hide has non-number value at cell number " cell-number)]
    if (cell-dist-to-hide < 0.0)
     [ error (word "cell-dist-to-hide has negative number at cell number " cell-number)]
  ]

end

to set-times-and-inputs

  ; Set time variables
  show "Setting time variables"
  set time time:create-with-format start-time "M/d/yyyy H:mm"
  set formatted-time time:show time "yyyy/MM/dd HH:mm"
  set end-time-code time:create-with-format end-time "M/d/yyyy H:mm"
  set next-output-time time:copy time  ; Always produce file output on first tick

  ; Some error checking
  if time:is-after time end-time-code [ error "Really, the simulation must end after it starts" ]
  output-print formatted-time

  ; Read all the input file's data into a LogoTimeSeries variable
  show "Reading time-series input file"
  if not file-exists? time-series-file-name [ error (word "Input file " time-series-file-name " not found") ]
  set input-time-series time:ts-load-with-format time-series-file-name "M/d/yyyy H:mm"

  ; Extract from the LogoTimeSeries a list of the time values in the input file
  set input-time-list time:ts-get-range input-time-series time end-time-code "LOGOTIME"
  ; Test output
  ; foreach input-time-list [ show time:show ? "MMMM d, yyyy HH:mm" ]

  ; Initialize the next-time variable
  set next-time 0
  set step-length 0.5 ; Becomes the initial value of prev-step-length

end

to tag-adjacent-cells
  ; A cell test procedure to color the patches of the adjacent cells
  ask adjacent-cells
  [
    ask cells-patches [ set pcolor pink ]
  ]

  ask cells-patches [ set pcolor white ]

end

to update-output
  ; Observer procedure to write outputs

  ; Display current time, flow, temperature
  output-print (word formatted-time " step length: " precision step-length 2 " flow: " flow " temperature: " temperature)

end
@#$#@#$#@
GRAPHICS-WINDOW
332
11
997
442
-1
-1
2.5
1
10
1
1
1
0
0
0
1
0
262
0
168
1
1
1
ticks
30.0

BUTTON
18
22
81
55
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
15
132
145
192
start-time
4/1/2010 00:00
1
0
String

INPUTBOX
13
203
146
263
end-time
8/1/2010 00:00
1
0
String

OUTPUT
334
470
901
546
12

BUTTON
91
22
154
55
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
160
22
223
55
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
#US Forest Service Santa Ana Sucker Model

## Units conventions

  * Length units are cm, for all habitat and fish variables, and for all outputs. The only exceptions are some inputs:

    * Cell polygon geometry, read in from shapefiles, are in meters (m). (See below for display distances.)

    * The depth and velocity lookup table input files use depths in m and velocities in m/s. These are converted to cm when read in.

    * Flow throughout is in m<sup>3</sup>/s.

  * Times are in days, although flow and velocity use seconds (s<sup>-1</sup>).

  * Temperatures are in degrees C.

  * Turbidity is in Nephelometric turbidity units (NTU).

## Other conventions

  * Display and model distances: Actual (model) distances are scaled for display to (a) make the world display a reasonable size, and (b) make sure each cell polygon includes at least one complete NetLogo patch. The ratio of patch size (width) to actual distance (in meters) is the global variable world-resolution: when world-resolution is 2.0, each patch represents 2 m of model distance. Hence, NetLogo distances (e.g., reported by primitives such as "distance") must be multiplied by (100 * world-resolution) to get model distances in cm.

  * "Cells" are turtles located at the centroid of the polygon they represent. Fish can be located anywhere in the polygon represented by their cell.

  * All habitat variables that are used by fish must be variables of the NetLogo patches within a cell, not variables of the cells. Fish access these variables from the patch they are on, not by querying the cells. **This works when evaluating variables of other cells, because the cell is a turtle that uses the variables of the underlying patch.**

  * The fishs' lists of survival and growth from previous time steps are maintained only if time step lengths are less than one day. For daily or longer time steps, these memories are not used for estimating future fitness.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count fishes</metric>
    <metric>mean [velocity] of model-patches with [ depth &gt; 0 ]</metric>
    <enumeratedValueSet variable="shade-variable">
      <value value="&quot;light&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-time">
      <value value="&quot;4/1/2010 00:00&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-time">
      <value value="&quot;4/1/2011 00:00&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="file-output?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-frames?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
