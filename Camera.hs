{-# LANGUAGE PackageImports #-}
module Camera where

import "linear" Linear

d0 = V3 0 (-1) 0
u0 = V3 0 0 (-1)
s0 p = (p,zero,zero,(0,0))

calcCam dt (dmx,dmy) (left,up,down,right,turbo) (p0,_,_,(mx,my)) = (p',p' + d,u,(mx',my'))
  where
    nil c n = if c then n else zero
    p'  = nil left (v ^* (-t)) + nil up (d ^* t) + nil down (d ^* (-t)) + nil right (v ^* t) + p0
    k   = if turbo then 500 else 100
    t   = k * realToFrac dt
    mx' = dmx-- + mx
    my' = dmy-- + my
    rm  = rotationEuler $ V3 (-mx' / 100) (-my' / 100) 0
    d   = rotate rm d0
    u   = rotate rm u0
    v   = signorm $ d `cross` u

rotationEuler :: V3 Float -> Quaternion Float
rotationEuler (V3 a b c) = axisAngle (V3 0 0 1) a * axisAngle (V3 1 0 0) b * axisAngle (V3 0 1 0) (c)
