module Direction (Direction(..)) where
import Prelude
data Direction = LeftDir | RightDir | UpDir | DownDir
derive instance eqDir :: Eq Direction