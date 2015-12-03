{-# LANGUAGE LambdaCase, RankNTypes, RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeFamilies                                           #-}
module Commands.Servers.Servant.API where
import           Commands.Extra 
import           Commands.Servers.Servant.Types
import           Commands.Servers.Servant.V 
import           Commands.Servers.Servant.API.Types 

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Wai
import           Servant
import           Servant.Client (client)

-- import           Control.Monad.IO.Class        (liftIO)
-- import Control.Concurrent.STM
-- import Data.Function ((&)) 


serveNatlink :: (Show a) => (VSettings m c a) -> IO ()
serveNatlink settings@VSettings{..} = do
 vSetup settings >>= \case
  Left e  -> do
   print e
  Right() -> do
   Wai.run vPort (natlinkApplication settings)

-- makeServantInterpreter :: -> Interpreter
-- makeServantInterpreter

natlinkApplication :: (Show a) => (VSettings m c a) -> Wai.Application
natlinkApplication vSettings = serve natlinkAPI (natlinkHandlers vSettings)

natlinkHandlers :: (Show a) => (VSettings m c a) -> Server NatlinkAPI
natlinkHandlers vSettings = postRecognition vSettings :<|> postHypotheses vSettings :<|> postCorrection vSettings :<|> postReload vSettings :<|> postContext vSettings -- TODO ReaderT

postRecognition :: (Show a) => (VSettings m c a) -> RecognitionRequest -> Response DNSResponse
-- postRecognition vSettings (RecognitionRequest ws) = (vSettings&vInterpretRecognition) vSettings ws
postRecognition vSettings = (vInterpretRecognition vSettings) vSettings 

{-| handle a hypothesis request, as a server  

-}
postHypotheses :: (Show a) => (VSettings m c a) -> HypothesesRequest -> Response DNSResponse
postHypotheses vSettings = (vInterpretHypotheses vSettings) vSettings 

postCorrection :: (Show a) => (VSettings m c a) -> CorrectionRequest -> Response DNSResponse 
postCorrection vSettings = (vInterpretCorrection vSettings) vSettings 

postReload :: (Show a) => (VSettings m c a) -> ReloadRequest -> Response DNSResponse 
postReload vSettings = (vInterpretReload vSettings) vSettings 

postContext :: (Show a) => (VSettings m c a) -> ContextRequest -> Response DNSResponse 
postContext vSettings = (vInterpretContext vSettings) vSettings 

{-| forward a hypothesis request, as a client  

-}
postHypothesesTo :: Address -> HypothesesRequest -> ClientResponse
postHypothesesTo address = client hypothesesClientAPI (address2baseurl address) 

