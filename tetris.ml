open UniverseJs
open Color
open Image
open World
open TransformToInt

type world_t = {
  zahyo1 : (int * int) list * Color.t;
  b : ((int * int) * Color.t) list;
  c : int list;
  e : int list;
}
type t = {
  a : (int * int) list;
}
type intlst = {
  d : int list;
}
let a v2 =
  if v2 = 0 then ([(370, 110); (390, 110); (410, 110); (430, 110); (390, 110)], Color.cyan)
  else if v2 = 1 then ([(390, 110); (410, 110); (390, 130); (410, 130); (400, 120)], Color.yellow)
  else if v2 = 2 then ([(370, 110); (390, 110); (390, 130); (410, 130); (390, 130)], Color.red)
  else if v2 = 3 then ([(390, 110); (410, 110); (370, 130); (390, 130); (390, 130)], Color.green)
  else if v2 = 4 then ([(390, 110); (370, 130); (390, 130); (410, 130); (390, 130)], make_color 255 30 200)
  else if v2 = 5 then ([(370, 110); (370, 130); (390, 130); (410, 130); (390, 130)], Color.blue)
  else ([(410, 110); (370, 130); (390, 130); (410, 130); (390, 130)], make_color 255 180 100)
let initial_world = {zahyo1 = a (Random.int 7); b = []; c = [7; Random.int 7; Random.int 7; Random.int 7; Random.int 7; Random.int 7; Random.int 7]; e = [0; 0; 0]}
let width = 1000
let height = 600
let f (x4, y5) =
  rectangle 20 20 y5
let g (x, y) =
  x
let h v4 v =
  let f (x9, y10) =
  let f v6 =
  (x9, y10 + 20) <> v6
  in y10 + 20 <= 490 && andmap f (List.map g v)
  in andmap f v4
let i (x2, y2) =
  (x2, y2 + 20)
let rec shadow v19 v26 =
  if h v19 v26 then shadow (List.map i v19) v26
  else v19
let add_lst {a = k1 :: (a2 :: (p1 :: (e2 :: h2)))} v34 =
  k1 :: (a2 :: (p1 :: (e2 :: v34)))
let rec draw_block1 {d = first5 :: rest2} =
  let s1 = f (a (first5 mod 8))
  in (if rest2 = [] then [s1; s1; s1; s1]
  else if first5 = 7 then draw_block1 {d = rest2}
  else s1 :: (s1 :: (s1 :: (s1 :: draw_block1 {d = rest2}))))
let draw_block {d = first3 :: (e1 :: (h1 :: (g1 :: (i1 :: (j1 :: (first :: rest))))))} =
  let f v3 v38 v41 =
  let f (x30, y3) =
  if v3 = 0 then (x30 + v38, y3 + v41 + 10)
  else if v3 = 1 then (x30 + v38, y3 + v41)
  else (x30 + v38 + 10, y3 + v41)
  in List.map f (g (a v3))
  in (let c2 = add_lst {a = f e1 250 30} (add_lst {a = f h1 350 30} (add_lst {a = f g1 450 30} (add_lst {a = f i1 450 130} (add_lst {a = f j1 450 230} (add_lst {a = f first 450 330} [])))))
  in (if first3 = 7 then c2
  else add_lst {a = f (first3 mod 8) (-250) 30} c2))
let draw {zahyo1 = ((first10 :: (b :: (n1 :: (d :: r1)))), y4); b = b2_v; c = v5; e = first7 :: (first8 :: (first16 :: rest4))} =
  let k = rectangle 20 20 y4
  in (let o1 = rectangle 20 20 (make_color 25 10 100 ~alpha:20)
  in (let i2 = rectangle 90 90 ~fill:false Color.black
  in place_images [rectangle 200 400 ~fill:false Color.black; i2; i2; i2; i2; i2; i2; i2; rectangle 20 20 ~fill:false Color.black] [(400, 300); (150, 150); (650, 150); (750, 150); (850, 150); (850, 250); (850, 350); (850, 450); (190, 500)] (place_images (List.map f b2_v) (List.map g b2_v) (place_images [k; k; k; k] [first10; b; n1; d] (place_images [o1; o1; o1; o1] (shadow [first10; b; n1; d] b2_v) (place_images (draw_block1 {d = v5}) (draw_block {d = v5}) (place_images (let r2 = [text ("score: " ^ string_of_int first8) 25 Color.black; text ("line: " ^ string_of_int first16) 25 Color.black; text "endless" 25 Color.black; text "hold" 20 Color.black; text "next" 20 Color.black]
  in (if first7 = 0 then r2
  else text "✔︎" 12 Color.black :: r2)) (let t2 = [(130, 300); (130, 350); (130, 500); (150, 90); (650, 90)]
  in (if first7 = 0 then t2
  else (190, 500) :: t2)) (empty_scene width height))))))))
let rec j v20 v11 v33 =
  if v20 > 490 then (v11, v33)
  else (let f1 ((x17, y22), y19) =
  y22 = v20
  in (let f2 v15 =
  let f3 (x12, y14) =
  x12 = v15
  in ormap f3 (List.map g (List.filter f1 v11))
  in (let s = v20 + 20
  in (if andmap f2 [310; 330; 350; 370; 390; 410; 430; 450; 470; 490] then (let f ((x22, y26), y24) =
  y26 <> v20
  in (let f4 ((x13, y15), y11) =
  if y15 < v20 then ((x13, y15 + 20), y11)
  else ((x13, y15), y11)
  in j s (List.map f4 (List.filter f v11)) (v33 + 1)))
  else j s v11 v33))))
let score v48 =
  if v48 = 1 then 10
  else if v48 = 2 then 30
  else if v48 = 3 then 50
  else if v48 = 4 then 80
  else 0
let rt v35 =
  v35 + 0
let nw {zahyo1 = ((a1 :: (m1 :: (t1 :: (v1 :: w1)))), y33); b = b7_v; c = first12 :: (u1 :: (y1 :: (x1 :: (z1 :: (b2 :: (first14 :: rest5)))))); e = first11 :: (first13 :: (first17 :: rest6))} =
  let f (x24, y18) =
  {zahyo1 = a u1; b = x24; c = [first12 mod 8; y1; x1; z1; b2; first14; Random.int 7]; e = (let g2 = first13 + score y18 + (if x24 = [] then 100
  else 0)
  in [first11; g2; first17 + y18])}
  in f (j 110 ((a1, y33) :: ((m1, y33) :: ((t1, y33) :: ((v1, y33) :: b7_v)))) 0)
let on_tick {zahyo1 = (x5, y6); b = b6_v; c = v31; e = v30} =
  if h x5 b6_v then {zahyo1 = (List.map i x5, y6); b = b6_v; c = v31; e = v30}
  else nw {zahyo1 = (x5, y6); b = b6_v; c = v31; e = v30}
let n v12 v13 v14 =
  let f (x11, y16) =
  let e = if v14 = "l" then x11 >= 310
  else x11 <= 490
  in (let f v32 =
  (x11, y16) <> v32
  in e && andmap f (List.map g v13))
  in andmap f v12
let rec p (c, (x18, (l, (z, (x23, y29))))) v16 v24 =
  if v24 = 4 then ""
  else (let f2 (x14, y20) =
  let f3 v10 =
  (x14, y20) <> v10
  in (if y20 < 110 then "u"
  else if y20 > 490 then "d"
  else if x14 < 310 then "l"
  else if x14 > 490 then "r"
  else if andmap f3 v16 then p (x18, (l, (z, (c, (x23, y29))))) v16 (v24 + 1)
  else if y20 > y29 then "d"
  else if x14 > x23 then "r"
  else if x14 < x23 then "l"
  else "u")
  in f2 c)
let rec o v8 v17 v22 (x16, (x25, (u, (c1, d1)))) v25 =
  if v25 > 5 then []
  else (let w = p (x16, (x25, (u, (c1, d1)))) (List.map g v17) 0
  in (if w = "" then v8
  else if ormap (let f v27 =
  v27 = "l"
  in f) v22 && ormap (let f v21 =
  v21 = "r"
  in f) v22 || ormap (let f v23 =
  v23 = "u"
  in f) v22 && ormap (let f v18 =
  v18 = "d"
  in f) v22 then []
  else (let f (x21, y28) =
  if w = "d" then (x21, y28 - 20)
  else if w = "l" then (x21 + 20, y28)
  else if w = "r" then (x21 - 20, y28)
  else (x21, y28 + 20)
  in o (List.map f v8) v17 (w :: v22) (f x16, (f x25, (f u, (f c1, f d1)))) (v25 + 1))))
let m {zahyo1 = ((first2 :: (first4 :: (r :: (t :: ((x20, y25) :: q))))), y21); b = b5_v; c = v29; e = v39} v9 =
  let f (x19, y23) =
  if v9 = "l" then (x20 - (y23 - y25), y25 + (x19 - x20))
  else (x20 + (y23 - y25), y25 - (x19 - x20))
  in (let b1 = o (List.map f [first2; first4; r; t; (x20, y25)]) b5_v [] (f first2, (f first4, (f r, (f t, (x20, y25))))) 0
  in (if b1 = [] then [first2; first4; r; t; (x20, y25)]
  else b1))
let c v28 =
  if v28 = Color.cyan then 0
  else if v28 = Color.yellow then 1
  else if v28 = Color.red then 2
  else if v28 = Color.green then 3
  else if v28 = make_color 255 30 200 then 4
  else if v28 = Color.blue then 5
  else 6
let hold {zahyo1 = (x28, y31); b = b8_v; c = first9 :: (j2 :: (l2 :: (k2 :: (m2 :: (first15 :: rest7))))); e = v40} =
  {zahyo1 = a first9; b = b8_v; c = [c y31 + 8; j2; l2; k2; m2; first15; Random.int 7]; e = v40}
let on_key {zahyo1 = (x15, y17); b = b3_v; c = first6 :: rest3; e = v42} key =
  if key = "S" && h x15 b3_v then {zahyo1 = (let f (x10, y12) =
  (x10, y12 + 20)
  in (List.map f x15, y17)); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "W" then nw {zahyo1 = (shadow x15 b3_v, y17); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "D" && (let f (x8, y13) =
  (x8 + 20, y13)
  in n (List.map f x15) b3_v "r") then {zahyo1 = (let f (x7, y9) =
  (x7 + 20, y9)
  in (List.map f x15, y17)); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "A" && (let f (x6, y8) =
  (x6 - 20, y8)
  in n (List.map f x15) b3_v "l") then {zahyo1 = (let f (x3, y7) =
  (x3 - 20, y7)
  in (List.map f x15, y17)); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "K" then {zahyo1 = (m {zahyo1 = (x15, y17); b = b3_v; c = first6 :: rest3; e = v42} "l", y17); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "L" then {zahyo1 = (m {zahyo1 = (x15, y17); b = b3_v; c = first6 :: rest3; e = v42} "r", y17); b = b3_v; c = first6 :: rest3; e = v42}
  else if key = "O" && first6 <= 7 then (if first6 = 7 then hold {zahyo1 = (x15, y17); b = b3_v; c = rest3; e = v42}
  else {zahyo1 = a first6; b = b3_v; c = c y17 + 8 :: rest3; e = v42})
  else {zahyo1 = (x15, y17); b = b3_v; c = first6 :: rest3; e = v42}
let on_mouse {zahyo1 = zahyo12_v; b = b4_v; c = c2_v; e = first19 :: rest9} mouse_x mouse_y event =
  let z2 = if mouse_x >= 180 && mouse_x <= 200 && mouse_y >= 490 && mouse_y <= 510 && event = "button_down" then 1 - first19
  else first19
  in {zahyo1 = zahyo12_v; b = b4_v; c = c2_v; e = z2 :: rest9}
let rate = 1000
let stop_when {zahyo1 = (x34, y37); b = b13_v; c = c7_v; e = first18 :: (l1 :: (d2 :: n2))} =
  let f (x26, y27) =
  (x26, y27 - 20)
  in not (h (List.map f x34) b13_v) || first18 = 0 && d2 >= 200
let to_draw_last {zahyo1 = zahyo13_v; b = b9_v; c = c3_v; e = first20 :: (q1 :: (q2 :: u2))} =
  let o2 (x33, y36) =
  place_images [text x33 50 y36; text ("score: " ^ string_of_int q1) 30 Color.black] [(500, 250); (500, 350)] (empty_scene width height)
  in (if first20 = 0 && q2 >= 200 then o2 ("CLEAR", Color.red)
  else o2 ("GAME OVER", Color.red))
;; big_bang initial_world
  ~width:width
  ~height:height
  ~to_draw:draw
  ~on_tick:on_tick
  ~on_mouse:on_mouse
  ~on_key_press:on_key
  ~rate:rate
  ~stop_when:stop_when
  ~to_draw_last:to_draw_last