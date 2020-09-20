(* file: setting.ml
 * author: Bob Muller
 *
 * A simple animation example, a setting sun over water.
 *
 * To run it:
 *
 * > cd src
 * > dune exec bin/main.exe
 *
*)
type model = int

let view _ = Image.circle 300. (Color.random ())

let update model = model + 1

let finished model = model > 20

let _ =
  Animate.start 0
    ~name: "Testing"
		~width: 600.
		~height: 600.
		~view: view
		~rate: 1.0
		~onTick: update
		~stopWhen: finished
		~viewLast: view
