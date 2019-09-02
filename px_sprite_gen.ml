(**
 * OCaml Pixel Sprite Generator v0.01
 *
 * This is an OCaml version of David Bollinger's pixelrobots and
 * pixelspaceships algorithm.
 *
 * This file is a rewrite of the Javascript version by Zelimir Fedoran.
 *
 * More info:
 * http://www.davebollinger.com/works/pixelrobots/
 * http://www.davebollinger.com/works/pixelspaceships/
 *
 * Archived website (links above are down):
 * http://web.archive.org/web/20080228054405/http://www.davebollinger.com/works/pixelrobots/
 * http://web.archive.org/web/20080228054410/http://www.davebollinger.com/works/pixelspaceships/
 *
 *)

type mask = {
  m_width   : int;
  m_height  : int;
  m_data    : int array;
  m_mirrorX : bool;
  m_mirrorY : bool;
}

(**
  The Mask class defines a 2D template form which sprites can be generated.

  @param {data} Integer array describing which parts of the sprite should be
    empty, body, and border. The mask only defines a semi-ridgid stucture
    which might not strictly be followed based on randomly generated numbers.

     -1 = Always border (black)
      0 = Empty
      1 = Randomly chosen Empty/Body
      2 = Randomly chosen Border/Body

  @param {width} Width of the mask data array
  @param {height} Height of the mask data array
  @param {mirrorX} A boolean describing whether the mask should be mirrored on the x axis
  @param {mirrorY} A boolean describing whether the mask should be mirrored on the y axis
*)
let create_mask ~data ~width ~height ~mirrorX ~mirrorY =
  { m_width   = width;
    m_height  = height;
    m_data    = data;
    m_mirrorX = mirrorX;
    m_mirrorY = mirrorY;
  }


type options = {
  colored         : bool;
  edgeBrightness  : float;
  colorVariations : float;
  brightnessNoise : float;
  saturation      : float;
}

let default_options = {
  colored         = true;
  edgeBrightness  = 0.3;
  colorVariations = 0.2;
  brightnessNoise = 0.3;
  saturation      = 0.5;
}

type sprite = {
  width   : int;
  height  : int;
  options : options;
  mask    : mask;
  mutable data : int array;
  mutable pixels : (int * int * int) array;
}

type rgb = {
  mutable r: float;
  mutable g: float;
  mutable b: float;
}


(**
  The getData function returns the sprite template data at location (x, y)

     -1 = Always border (black)
      0 = Empty
      1 = Randomly chosen Empty/Body
      2 = Randomly chosen Border/Body
*)
let getData = (fun this x y ->
  this.data.(y * this.width + x)
)


(**
  The setData function sets the sprite template data at location (x, y)

     -1 = Always border (black)
      0 = Empty
      1 = Randomly chosen Empty/Body
      2 = Randomly chosen Border/Body
*)
let setData = (fun this x y value ->
  this.data.(y * this.width + x) <- value;
)


(**
  The mirrorX function mirrors the template data horizontally.
*)
let mirrorX = (function this ->
  let h = this.height in
  let w = this.width / 2 in
  for y = 0 to pred h do
    for x = 0 to pred w do
      setData this (this.width - x - 1) y (getData this x y);
    done
  done
)


(**
  The mirrorY function mirrors the template data vertically.
*)
let mirrorY = (function this ->
  let h = this.height / 2 in
  let w = this.width in
  for y = 0 to pred h do
    for x = 0 to pred w do
      setData this x (this.height - y - 1) (getData this x y);
    done
  done
)


(**
  The applyMask function copies the mask data into the template data array at
  location (0, 0).

  (note: the mask may be smaller than the template data array)
*)
let applyMask = (function this ->
  let h = this.mask.m_height in
  let w = this.mask.m_width in

  for y = 0 to pred h do
    for x = 0 to pred w do
      setData this x y (this.mask.m_data.(y * w + x));
    done
  done
)


(**
  The initData function initializes the sprite data to completely solid.
*)
let initData = (function this ->
  let h = this.height in
  let w = this.width in
  for y = 0 to pred h do
    for x = 0 to pred w do
      setData this x y (-1);
    done
  done
)


(**
  Apply a random sample to the sprite template.

  If the template contains a 1 (internal body part) at location (x, y), then
  there is a 50% chance it will be turned empty. If there is a 2, then there
  is a 50% chance it will be turned into a body or border.

  (feel free to play with this logic for interesting results)
*)
let generateRandomSample = (function this ->
  let h = this.height in
  let w = this.width in

  for y = 0 to pred h do
    for x = 0 to pred w do
      let v = getData this x y in

      let v =
        if v = 1 then
          v * (Random.int 2)
        else if v = 2 then begin
          if (Random.float 1.0) > 0.5
          then 1
          else -1
        end
        else v
      in

      setData this x y v;
    done
  done
)


(**
  This function applies edges to any template location that is positive in
  value and is surrounded by empty (0) pixels.
*)
let generateEdges = (function this ->
  let h = this.height in
  let w = this.width in

  for y = 0 to pred h do
    for x = 0 to pred w do
      if (getData this x y) > 0 then begin
        if y - 1 >= 0 && (getData this x (y-1)) = 0 then
          setData this x (y-1) (-1);

        if y + 1 < this.height && (getData this x (y+1)) = 0 then
          setData this x (y+1) (-1);

        if x - 1 >= 0 && (getData this (x-1) y) = 0 then
          setData this (x-1) y (-1);

        if (x + 1 < this.width) && (getData this (x+1) y = 0) then
          setData this (x+1) y (-1);
      end
    done
  done
)


(**
  This function converts HSL color values to RGB color values.
*)
let hslToRgb = (fun h s l rgb ->

  let i = floor (h *. 6.) in
  let f = h *. 6. -. i in
  let p = l *. (1. -. s) in
  let q = l *. (1. -. f *. s) in
  let t = l *. (1. -. (1. -. f) *. s) in

  (*
  match (int_of_float i) mod 6 with
  | 0 -> { r = l; g = t; b = p; }
  | 1 -> { r = q; g = l; b = p; }
  | 2 -> { r = p; g = l; b = t; }
  | 3 -> { r = p; g = q; b = l; }
  | 4 -> { r = t; g = p; b = l; }
  | 5 -> { r = l; g = p; b = q; }
  | _ -> assert false
  *)
  let r, g, b =
    match (int_of_float i) mod 6 with
    | 0 -> (l, t, p)
    | 1 -> (q, l, p)
    | 2 -> (p, l, t)
    | 3 -> (p, q, l)
    | 4 -> (t, p, l)
    | 5 -> (l, p, q)
    | _ -> assert false
  in
  rgb.r <- r;
  rgb.g <- g;
  rgb.b <- b;
)


(**
  This function renders out the template data to a HTML canvas to finally
  create the sprite.

  (note: only template locations with the values of -1 (border) are rendered)
*)
let renderPixelData = (function this ->

  let isVerticalGradient = Random.float 1.0 > 0.5 in
  let saturation = max (min ((Random.float 1.0) *. this.options.saturation) 1.0) 0.0 in
  let hue = Random.float 1.0 in

  let ulen, vlen =
    if isVerticalGradient
    then (this.height, this.width)
    else (this.width, this.height)
  in

  for u = 0 to pred ulen do
    (* Create a non-uniform random number between 0 and 1 (lower numbers more likely) *)
    let isNewColor = abs_float (( ((Random.float 1.0) *. 2. -. 1.) +.
                                  ((Random.float 1.0) *. 2. -. 1.) +.
                                  ((Random.float 1.0) *. 2. -. 1.) ) /. 3.) in

    (* Only change the color sometimes (values above 0.8 are less likely than others) *)
    let hue =
      if isNewColor > (1.0 -. this.options.colorVariations)
      then Random.float 1.0
      else hue
    in

    for v = 0 to pred vlen do

      let va, index =
        if isVerticalGradient then
          (getData this v u,
           (u * vlen + v))
        else
          (getData this u v,
           (v * ulen + u))
      in

      let rgb = { r = 1.0; g = 1.0; b = 1.0 } in

      if va <> 0 then begin
        if this.options.colored then begin
          (* Fade brightness away towards the edges *)
          let brightness = sin ((float u /. float ulen) *. Float.pi)
                         *. (1.0 -. this.options.brightnessNoise) 
                         +. (Random.float 1.0) *. this.options.brightnessNoise in

          (* Get the RGB color value *)
          hslToRgb hue saturation brightness rgb;

          (* If this is an edge, then darken the pixel *)
          if va = -1 then begin
            rgb.r <- rgb.r *. this.options.edgeBrightness;
            rgb.g <- rgb.g *. this.options.edgeBrightness;
            rgb.b <- rgb.b *. this.options.edgeBrightness;
          end;

        end else begin
          (* Not colored, simply output black *)
          if va = -1 then begin
            rgb.r <- 0.;
            rgb.g <- 0.;
            rgb.b <- 0.;
          end
        end
      end;

      this.pixels.(index) <-
        (int_of_float (rgb.r *. 255.),
         int_of_float (rgb.g *. 255.),
         int_of_float (rgb.b *. 255.));
    done
  done
)


(**
  The init function calls all functions required to generate the sprite.
*)
let init = (function this ->
  initData this;

  applyMask this;
  generateRandomSample this;

  if this.mask.m_mirrorX then
    mirrorX this;

  if this.mask.m_mirrorY then
    mirrorY this;

  generateEdges this;
  renderPixelData this;
)


(**
  The Sprite class makes use of a Mask instance to generate a 2D sprite on a
  HTML canvas.

  let options = {
    colored         = true;   (* boolean *)
    edgeBrightness  = 0.3;    (* value from 0.0 to 1.0 *)
    colorVariations = 0.2;    (* value from 0.0 to 1.0 *)
    brightnessNoise = 0.3;    (* value from 0.0 to 1.0 *)
    saturation      = 0.5;    (* value from 0.0 to 1.0 *)
  }

  @param {mask}
  @param {options} 
*)
let create_sprite mask ?options () =
  let width = mask.m_width * (if mask.m_mirrorX then 2 else 1) in
  let height = mask.m_height * (if mask.m_mirrorY then 2 else 1) in
  let this = {
    width; height;  mask;
    data = Array.make (width * height) 0;
    pixels = Array.make (width * height) (0, 0, 0);
    options =
      match options with
      | None -> default_options
      | Some op -> op;
  } in
  init (this);
  (this)


(* example of use *)
let () =
  Random.self_init ();
  Graphics.open_graph "";
  let w, h =
    (8 + 36 * 16 + 8),
    (8 + 22 * 16 + 8)
  in
  Graphics.resize_window w h;
  Graphics.set_window_title "Pixel Sprite Generator";
  for i = 0 to pred 36 do
    for j = 0 to pred 22 do
      let spaceship_mask =
        create_mask ~width:6 ~height:12 ~data:[|
          0; 0; 0; 0; 0; 0;
          0; 0; 0; 0; 1; 1;
          0; 0; 0; 0; 1;-1;
          0; 0; 0; 1; 1;-1;
          0; 0; 0; 1; 1;-1;
          0; 0; 1; 1; 1;-1;
          0; 1; 1; 1; 2; 2;
          0; 1; 1; 1; 2; 2;
          0; 1; 1; 1; 2; 2;
          0; 1; 1; 1; 1;-1;
          0; 0; 0; 1; 1; 1;
          0; 0; 0; 0; 0; 0;
        |] ~mirrorX:true
           ~mirrorY:false
      in
      let options = { default_options with colored = true } in
      let sprite = create_sprite spaceship_mask ~options () in
      let px = Array.map (fun (r, g, b) -> Graphics.rgb r g b) sprite.pixels in
      let img_px =
        Array.init 12 (fun y ->
          Array.init 12 (fun x ->
            px.(y * 12 + x)
          )
        )
      in
      let img = Graphics.make_image img_px in
      Graphics.draw_image img (8 + i * 16) (8 + j * 16);
    done;
  done;
  let _ = Graphics.read_key () in
  Graphics.close_graph ()
