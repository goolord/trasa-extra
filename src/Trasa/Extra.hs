{-# LANGUAGE 
    LambdaCase 
  , OverloadedStrings
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC -Wall #-}

module Trasa.Extra 
  ( -- * Route Functions
  , module Trasa.Core.Implicit
  , encodeRoute
  , decodeRoute
  , redirect
  , getQueryString
    -- * Header Functions
  , getHeader
  , currentHeader
  , setHeader
  , getCookies
  , lookupCookie
  , setCookie
    -- * Codecs and Parsing
  , pathPieceCodec
  , bodyAeson
  , aeson
  , decodeInt
    -- * Errors
  , err404
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (ToJSON(..), FromJSON(..), encode, eitherDecode', decode')
import Data.Bifunctor (first)
import Data.CaseInsensitive
import Data.Text (Text)
import Network.HTTP.Types as HTTP
import Trasa.Core hiding (optional)
import Trasa.Core.Implicit
import Trasa.Server
import Web.PathPieces
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as TR
import qualified Trasa.Method
import qualified Web.Cookie as Cookie

encodeRoute :: HasMeta route => Concealed route -> Text 
encodeRoute c = encodeUrl $ link c

decodeRoute :: HasMeta route => Router route -> Text -> Maybe (Concealed route)
decodeRoute router t = either (const Nothing) Just parse

redirect :: HasMeta route => Prepared route response -> LBS.ByteString -> TrasaT IO a
redirect route message = do
  setHeader "Location" (encodeRoute $ conceal route)
  throwError $ TrasaErr HTTP.status302 message

getQueryString :: MonadIO m => TrasaT m QueryString
getQueryString = fmap trasaQueryString ask

getHeader :: CI BS.ByteString -> TrasaT IO (Maybe T.Text)
getHeader idt = fmap (M.lookup idt . trasaHeaders) ask

currentHeader :: CI BS.ByteString -> TrasaT IO (Maybe T.Text)
currentHeader idt = fmap (M.lookup idt) get

setHeader :: CI BS.ByteString -> T.Text -> TrasaT IO ()
setHeader idt header = modify (M.insert idt header)

getCookies :: TrasaT IO (Maybe Cookie.Cookies)
getCookies = getHeader "Cookie" >>= \case
  Nothing -> pure Nothing
  Just rawCookie -> pure $ Just $ Cookie.parseCookies $ T.encodeUtf8 rawCookie

lookupCookie :: BS.ByteString -> TrasaT IO (Maybe BS.ByteString)
lookupCookie name = do
  getCookies >>= \case 
    Nothing -> pure Nothing
    Just cookies -> pure $ lookup name cookies

setCookie :: Cookie.SetCookie -> TrasaT IO ()
setCookie cookie = do
  let cookie' :: Text
      cookie' = T.decodeUtf8 $ BL.toStrict $ BB.toLazyByteString $ Cookie.renderSetCookie cookie
  setHeader "Set-Cookie" cookie'

pathPieceCodec :: PathPiece piece => CaptureCodec piece
pathPieceCodec = CaptureCodec toPathPiece fromPathPiece

bodyAeson :: (ToJSON a, FromJSON a) => BodyCodec a
bodyAeson = BodyCodec (pure "application/json") encode (first T.pack . eitherDecode')

aeson :: (ToJSON a, FromJSON a) => CaptureCodec a
aeson = CaptureCodec (T.decodeUtf8 . LBS.toStrict . encode) (decode' . LBS.fromStrict .  T.encodeUtf8)

decodeInt :: Text -> Maybe Int
decodeInt x = case TR.decimal x of
  Left _ -> Nothing
  Right (i,leftover) -> if T.null leftover then Just i else Nothing

err404 :: Monad m => TrasaT m a
err404 = throwError (TrasaErr HTTP.status404 "Not found")

