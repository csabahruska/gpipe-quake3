module Camera where

import Data.Vect

d0 = Vec4 0 (-1) 0 1
u0 = Vec4 0 0 (-1) 1
s0 p = (p,zero,zero,(0,0))

calcCam dt (dmx,dmy) (left,up,down,right,turbo) (p0,_,_,(mx,my)) = (p',p'&+d,u,(mx',my'))
  where
    nil c n = if c then n else zero
    p'  = nil left (v &* (-t)) &+ nil up (d &* t) &+ nil down (d &* (-t)) &+ nil right (v &* t) &+ p0
    k   = if turbo then 500 else 100
    t   = k * realToFrac dt
    mx' = dmx-- + mx
    my' = dmy-- + my
    rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
    d   = trim $ rm *. d0 :: Vec3
    u   = trim $ rm *. u0 :: Vec3
    v   = normalize $ d &^ u

rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

