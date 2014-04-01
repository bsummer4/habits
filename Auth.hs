{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}

module Auth
  ( User, Password, Token, Registrations
  , mkPass, emptyRegistrations
  , register, authenticate, token
  , module Web.ClientSession
  ) where

import ClassyPrelude
import Prelude.Unicode
import qualified Crypto.PasswordStore as PW
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import Web.ClientSession
import Data.Acid
import Data.SafeCopy
import Control.Monad.State (put)
import Control.Monad.Reader (ask)

data Password = Password ByteString
type User = ByteString
type Token = ByteString
data Registrations = Regs(Map User ByteString)

emptyRegistrations ∷ Registrations
emptyRegistrations = Regs $ Map.empty

deriving instance Typeable Registrations
$(deriveSafeCopy 0 'base ''Registrations)

mkPass ∷ Text → Password
mkPass = Password ∘ Text.encodeUtf8


-- [[AcidState Stuff]]
insDB ∷ User → ByteString → Update Registrations ()
insDB u pwhash = liftQuery ask >>= \(Regs r) →
  put $ Regs $ Map.insert u pwhash r

queryDB ∷ Query Registrations Registrations
queryDB = ask

$(makeAcidic ''Registrations ['insDB, 'queryDB])


-- [[Auth Stuff]]
token ∷ Key → User → Password → IO Token
token k u (Password p) =
  encryptIO k $ unlazy $ J.encode(Text.decodeUtf8 u, Text.decodeUtf8 p)

authenticate ∷ AcidState Registrations → Key → Token → IO(Maybe User)
authenticate db k tok = do
  Regs regs ← query db $ QueryDB
  return $ do
    tokjson ← decrypt k tok
    (userT,pass) ← J.decode $ tolazy tokjson
    let user = encodeUtf8 userT
    pwhash ← Map.lookup user regs
    let allgood = PW.verifyPassword (Text.encodeUtf8 pass) pwhash
    if not allgood then Nothing else
      Just user

register ∷ AcidState Registrations → User → Password → IO Bool
register db user (Password p) = do
  pwhash ← PW.makePassword p 14
  Regs regs ← query db $ QueryDB
  if Map.member user regs then return False else do
    () ← update db $ InsDB user pwhash
    return True


-- [[Utillities]]
tolazy ∷ BS.ByteString → LBS.ByteString
tolazy b = LBS.fromChunks [b]

unlazy ∷ LBS.ByteString → BS.ByteString
unlazy = BS.concat ∘ LBS.toChunks
