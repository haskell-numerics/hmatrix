-- conversion to/from Data.Vector.Storable
-- from Roman Leshchinskiy "vector" package
--
-- In the future Data.Packed.Vector will be replaced by Data.Vector.Storable

-------------------------------------------

import Data.Packed as H
import Data.Packed.Development(unsafeFromForeignPtr, unsafeToForeignPtr)
import Foreign.Storable
import qualified Data.Vector.Storable as V

fromVector :: Storable t => V.Vector t -> H.Vector t
fromVector v = unsafeFromForeignPtr p i n where
    (p,i,n) = V.unsafeToForeignPtr (V.copy v)

toVector :: H.Vector t -> V.Vector t
toVector v = V.unsafeFromForeignPtr p i n where
    (p,i,n) = unsafeToForeignPtr v

-------------------------------------------

v = V.slice 5 10 (V.fromList [1 .. 10::Double] V.++ V.replicate 10 7)

w = linspace 5 (0,2)

main = do
    print $ fromVector v
    print $ toVector w
