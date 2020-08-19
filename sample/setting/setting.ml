(* file: setting.ml
 * author: Bob Muller
 *
 * A simple animation example, a setting sun over water.
 *
 * To run it:
 * > make
 * > ./go
 *
 *)
open Code

let displayWidth = 600
let displayHeight = displayWidth

let radius = displayWidth / 6
let dropAmount = 10
let rightShiftAmount = 3

let sky   = Image.rectangle displayWidth displayHeight Color.lightBlue
let water = Image.rectangle displayWidth (displayHeight / 2) Color.blue
let sun   = Image.circle radius Color.orange

(* Just need to keep track of changing y-component of sun.
*)
type model = { x : int
             ; y : int
             }
let initialModel = { x = displayWidth / 2 - radius
                   ; y = 0
                   }

let view model =
  let sky = Image.place_image sun (model.x, model.y - radius) sky
  in
  Image.place_image water (0.0, displayHeight - radius) sky

let dropSun model = { x = model.x + rightShiftAmount
                    ; y = model.y + dropAmount
                    }

let sunHasSet model = model.y >= displayHeight

let _ =
  Animate.start initialModel
    ~name: "Setting Sun"
		~width: displayWidth
		~height: displayHeight
		~view: view
		~rate: 0.5
		~onTick: dropSun
		~stopWhen: sunHasSet
		~viewLast: view
