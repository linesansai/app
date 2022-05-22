module Core.Utils where
import Core.AppMonad (MonadApp)
import System.Random (randomIO)
import qualified Data.UUID as UUID
import Core.Types.UUID (UUID (..))
import qualified Data.Text as T

genUUID ::  MonadApp m => m UUID
genUUID = UUID . UUID.toText <$> randomIO 

tshow :: Show a => a -> T.Text
tshow = T.pack . show