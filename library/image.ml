(* filename : image.ml *)

type t =
    Rect of { color : Color.t
            ; x1 : int; y1 : int
            ; x2 : int; y2 : int
            ; fill : bool
            ; outline_size : int
            }
  | Polygon of { color : Color.t
               ; x : int
               ; y : int
               ; points : (int * int) list
               ; fill : bool
               ; outline_size : int
               }
  | Circle of { color : Color.t
              ; x1 : int; y1 : int
              ; radius : int
              ; fill : bool
              ; outline_size : int
              }
  | Line of { color : Color.t
            ; size : float
            ; x : int
            ; y : int
            ; points : (int * int) list
            }
  | Text of { color : Color.t
            ; text : string
            ; x : int
            ; y : int
            ; size : float
            }
  | Picture of { picture : Cairo.Surface.t
               ; x : int
               ; y : int
               }
  | Images of t list

(* relativeToAbsolute : (int * int) list -> int * int -> (int * int) list *)
let rec relativeToAbsolute points (previousX, previousY) =
  match points with
  | [] -> []
  | (x, y) :: points ->
    let absX, absY = x + previousX, y + previousY
    in
    (absX, absY) :: relativeToAbsolute points (absX, absY)

(* get_min : (int * int) list -> (int * int) -> (int * int)
   NB: points are expected to be absolute points not relative,
       this function returns the upper left corner of the
       bounding box for the list of points. *)
let rec get_min points (minx, miny) =
  match points with
  | [] -> (minx, miny)
  | (x, y) :: points ->
    let nextminx = if minx <= x then minx else x in
    let nextminy = if miny <= y then miny else y
    in
    get_min points (nextminx, nextminy)

(* get_max : (int * int) list -> (int * int) -> (int * int)
   NB: points are expected to be absolute points not relative *)
let rec get_max points (maxx, maxy) =
  match points with
  | [] -> (maxx, maxy)
  | (x, y) :: rest ->
    let nextmaxx = if maxx >= x then maxx else x in
    let nextmaxy = if maxy >= y then maxy else y
    in
    get_max rest (nextmaxx, nextmaxy)

(* text_wh : string -> float -> (int * int) *)
let text_wh str size =
  let surface = Cairo.Image.create Cairo.Image.ARGB32 ~w:0 ~h:0 in
  let context = Cairo.create surface in
  let () = Cairo.set_font_size context size in
  let Cairo.{width; height} = Cairo.text_extents context str
  in
  (int_of_float width, int_of_float height)

(* move_xy : (int * int) list -> (int * int) *)
let move_xy points =
  let absolutePoints = relativeToAbsolute points (0, 0) in
  let (min_x, min_y) = get_min absolutePoints (0, 0) in
  let move_x = if min_x >= 0 then 0 else min_x in
  let move_y = if min_y >= 0 then 0 else min_y
  in
  (move_x, move_y)

(* rectangle : float -> float ->
               ?fill:bool -> ?outline_size:float -> Color.t -> Image.t *)
let rectangle width height ?(fill=true) ?(outline_size=0) color =
  Rect { color
       ; x1 = 0; y1 = 0
       ; x2 = width; y2 = height
       ; fill
       ; outline_size
       }

(* empty : int -> int -> Image.t *)
let empty width height =
  rectangle width height Color.white ~fill:true ~outline_size:0

(* circle : int -> ?fill:bool -> ?outline_size:int -> Color.t -> Image.t *)
let circle radius ?(fill=true) ?(outline_size=0) color =
  Circle { color
         ; x1 = radius
         ; y1 = radius
         ; radius
         ; fill
         ; outline_size
         }

(* line : (int * int) list -> ?size:float -> Color.t -> Image.t *)
let line points ?(size=0.5) color =
  let (moveX, moveY) = move_xy points
  in
  Line { color
       ; size
       ; x = 0 - moveX
       ; y = 0 - moveY
       ; points
       }

(* polygon : (int * int) list ->
             ?fill:bool -> ?outline_size:int -> Color.t -> Image.t *)
let polygon points ?(fill=true) ?(outline_size=0) color =
  let (moveX, moveY) = move_xy points
  in
  Polygon { color
          ; x = 0 - moveX
          ; y = 0 - moveY
          ; points
          ; fill
          ; outline_size
          }

(* text : string -> ?size:float -> Color.t -> Image.t *)
let text str ?(size=20.) color =
  let (_, height) = text_wh str size
  in
  Text { color
       ; text = str
       ; x = 0
       ; y = height
       ; size
       }

(* readImage : string -> Image.t *)
let readImage name =
  let picture =
    try
      Cairo.PNG.create name
    with Out_of_memory ->
      failwith ("readImage: The file " ^ name ^ " is not PNG.")
  in
  Picture { picture
          ; x = 0
          ; y = 0
          }

(* moveImage : int -> int -> Image.t -> Image.t *)
let rec moveImage dx dy image =
  match image with
  | Rect {color; x1; y1; x2; y2; fill; outline_size} ->
    Rect { color
         ; x1 = x1 + dx; y1 = y1 + dy
         ; x2; y2
         ; fill
         ; outline_size
         }
  | Polygon {color; x; y; points; fill; outline_size} ->
    Polygon { color
            ; x = x + dx
            ; y = y + dy
            ; points
            ; fill
            ; outline_size
            }
  | Circle {color; x1; y1; radius; fill; outline_size} ->
    Circle { color
           ; x1 = x1 + dx
           ; y1 = y1 + dy
           ; radius
           ; fill
           ; outline_size
           }
  | Line {color; size; x; y; points} ->
    Line { color
         ; size
         ; x = x + dx
         ; y = y + dy
         ; points
         }
  | Text {color; text; x; y; size} ->
    Text { color
         ; text
         ; x = x + dx
         ; y = y + dy
         ; size
         }
  | Picture {picture; x; y} ->
    Picture { picture
            ; x = x + dx
            ; y = y + dy
            }
  | Images images -> Images (List.map (moveImage dx dy) images)

(* placeImages : Image.t list -> (int * int) list -> Image.t -> Image.t *)
let rec placeImages images positions background =
  match (images, positions) with
  | ([], []) -> background
  | ([], _) -> failwith "place_images: too many posns."
  | (_ , []) -> failwith "place_images: too many pics."
  | (image :: images, (dx, dy) :: positions) ->
    let image = moveImage dx dy image in
    let images =
      let background = placeImages images positions background
      in
      match background with
      | Images images -> images
      | _ -> [background]
    in
    Images (image :: images)

(* placeImage : Image.t -> int * int -> Image.t -> Image.t *)
let rec placeImage image position background =
  placeImages [image] [position] background

(* rev_iter *)
let rec rev_iter f lst =
  match lst with
  | [] -> ()
  | first :: rest ->
    rev_iter f rest;
    f first

(* draw_outline : Cairo.context -> int -> unit *)
let drawOutline context outline_size =
  Cairo.set_line_width context (float outline_size);
  Cairo.set_line_cap context Cairo.ROUND;
  Cairo.set_line_join context Cairo.JOIN_ROUND;
  Cairo.stroke context

(* drawLine : (int * int) list -> Cairo.context -> unit *)
let rec drawLine points context =
  match points with
  | [] -> ()
  | (x, y) :: points ->
    Cairo.rel_line_to context (float x) (float y);
    drawLine points context

(* rgba color components 0. - 255. Here they are percentages
   so, e.g., r = 127 would be ~50% red. *)
let percentage x = float_of_int x /. 255.
let percentagesOf255 (r, g, b, a) =
  (percentage r, percentage g, percentage b, percentage a)

(* draw : Cairo.context -> t -> unit *)
let rec draw context image =
  match image with
  | Rect {color; x1; y1; x2; y2; fill; outline_size} ->
    let w = float x2 in
    let h = float y2 in
    let (r, g, b, a) = percentagesOf255 (Color.to_rgba color)
    in
    Cairo.set_source_rgba context r g b a;
    Cairo.rectangle context (float x1) (float y1) ~w ~h;
    if fill then Cairo.fill_preserve context;
    drawOutline context outline_size

  | Polygon {color; x; y; points; fill; outline_size} ->
    let (r, g, b, a) = percentagesOf255 (Color.to_rgba color)
    in
      Cairo.set_source_rgba context r g b a;
      Cairo.move_to context (float x) (float y);
      drawLine points context;
      Cairo.Path.close context;
      if fill then Cairo.fill_preserve context;
      drawOutline context outline_size

  | Circle {color; x1; y1; radius; fill; outline_size} ->
    let x1, y1, radius = float x1, float y1, float radius in
    let pi2 = 8. *. atan 1. in
    let (r, g, b, a) = percentagesOf255 (Color.to_rgba color)
    in
    Cairo.set_source_rgba context r g b a;
    Cairo.arc context x1 y1 ~r:radius ~a1:0. ~a2:pi2;
    if fill then Cairo.fill_preserve context;
    drawOutline context outline_size

  | Line {color; size; x; y; points} ->
    let (r, g, b, a) = percentagesOf255 (Color.to_rgba color)
    in
    Cairo.set_source_rgba context r g b a;
    Cairo.set_line_width context size;
    Cairo.move_to context (float x) (float y);
    drawLine points context;
    Cairo.set_line_cap context Cairo.ROUND;
    Cairo.set_line_join context Cairo.JOIN_ROUND;
    Cairo.stroke context

  | Text {color; text; x; y; size} ->
    let (r, g, b, a) = percentagesOf255 (Color.to_rgba color)
    in
    Cairo.set_source_rgba context r g b a;
    Cairo.set_font_size context size;
    Cairo.move_to context (float x) (float y);
    Cairo.show_text context text;
    Cairo.fill context

  | Picture {picture; x; y} ->
    let x, y = float x, float y in
    Cairo.set_source_surface context picture ~x ~y;
    Cairo.paint context

  | Images images -> rev_iter (draw context) images

(* get_corner_posn : ((int * int) * (int * int)) list
                     -> (int * int) * (int * int) *)
let rec get_corner_posn lst ((minx, miny), (maxx, maxy)) =
  match lst with
  | [] -> ((minx, miny), (maxx, maxy))
  | ((x1, y1), (x2, y2)) :: rest ->
    let nextminx = if minx <= x1 then minx else x1 in
    let nextminy = if miny <= y1 then miny else y1 in
    let nextmaxx = if maxx >= x2 then maxx else x2 in
    let nextmaxy = if maxy >= y2 then maxy else y2
    in
    get_corner_posn rest ((nextminx, nextminy), (nextmaxx, nextmaxy))

(* get_min_max : (int * int) list -> ((int * int) * (int * int)) *)
let rec get_min_max points ((minx, miny), (maxx, maxy)) =
  let upperLeft  = get_min points (minx, miny) in
  let lowerRight = get_max points (maxx, maxy)
  in
  (upperLeft, lowerRight)

(* lower_left : (int * int) -> int -> (int * int) *)
let lower_left (x, y) distance =
  (x - distance, y - distance)

(* upper_right : (int * int) -> int -> (int * int) *)
let upper_right (x, y) distance =
  (x + distance, y + distance)

(* make_corner_posn : (int * int) -> (int * int) list ->
                      ((int * int) * (int * int)) list *)
let make_corner_posn (x, y) points =
  let ab_lst = relativeToAbsolute points (x, y)
  in
  get_min_max ab_lst ((x, y), (x, y))

(* make_corners : (int * int) * (int * int) -> int ->
                  (((int * int) * int) * (int * int)) list *)
let make_corners (min_posn, max_posn) outline_size =
  let half_size = outline_size / 2
  in
  ((lower_left min_posn half_size, half_size),
   upper_right max_posn half_size)

(* corner_lst : Image.t list -> (((int * int) * int) * (int * int)) list *)
let rec corner_lst images =
  match images with
  | [] -> []
  | Rect {color; x1; y1; x2 = w; y2 = h; fill; outline_size} :: rest ->
    let move = outline_size / 2 in
    let first = ((x1 - move, y1 - move), move) in
    let second = (x1 + w + move, y1 + h + move)
    in
    (first, second) :: corner_lst rest
  | Polygon {color; x; y; points; fill; outline_size} :: rest ->
    let min_max = make_corner_posn (x, y) points
    in
    (make_corners min_max outline_size) :: corner_lst rest
  | Circle {color; x1; y1; radius; fill; outline_size} :: rest ->
    let move = outline_size / 2 in
    let first = ((x1 - radius - move, y1 - radius - move), move) in
    let second = (x1 + radius + move, y1 + radius + move)
    in
    (first, second) :: corner_lst rest
  | Line {color; size; x; y; points} :: rest ->
    let size = int_of_float size in
    let min_max = make_corner_posn (x, y) points
    in
    (make_corners min_max size) :: corner_lst rest
  | Text {color; text; x; y; size} :: rest ->
    let (width, height) = text_wh text size
    in
    (((x, y), 0), (x + width, y + height)) :: corner_lst rest
  | Picture {picture; x; y} :: rest ->
    let x' = x + Cairo.Image.get_width picture in
    let y' = y + Cairo.Image.get_height picture
    in
    (((x, y), 0), (x', y')) :: corner_lst rest
  | (Images images) :: _ -> corner_lst images

(* imageToSurface : Image.t -> (Cairo.Surface.t * int) *)
let imageToSurface image =
  let posnlst = corner_lst [image] in
  let (((minx, miny), move_distance), (maxx, maxy)) =
    get_corner_posn (List.tl posnlst) (List.hd posnlst) in
  let surface = Cairo.Image.create Cairo.Image.ARGB32
                                   ~w:(maxx - minx)
                                   ~h:(maxy - miny) in
  let context = Cairo.create surface
  in
  draw context (moveImage move_distance move_distance image);
  (surface, move_distance)

(* to_bitmap : Image.t -> Cairo.Image.data32 *)
let toBitmap image =
  let (surface, _) = imageToSurface image
  in
  Cairo.Image.get_data32 surface

(* fromBitmap : Cairo.Image.data32 -> Image.t *)
let fromBitmap data =
  Picture { picture = Cairo.Image.create_for_data32 data
          ; x = 0
          ; y = 0
          }

(* freeze : Image.t -> Image.t *)
let freeze image =
  let (picture, move_distance) = imageToSurface image in
  let move_distance = 0 - move_distance
  in
  Picture { picture
          ; x = move_distance
          ; y = move_distance
          }
