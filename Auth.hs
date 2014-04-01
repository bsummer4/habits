{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Auth
  ( User, Password, Token, Registrations
  , mkPass, register, authenticate, token
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

data Password = Password ByteString
type User = Text
type Token = ByteString
data Registrations = Regs(Map User ByteString)

mkPass ∷ Text → Password
mkPass = Password ∘ Text.encodeUtf8

token ∷ Key → Text → Password → IO Token
token k u (Password p) =
	encryptIO k $ unlazy $ J.encode(u,Text.decodeUtf8 p)

authenticate ∷ Key → Registrations → Token → Maybe User
authenticate k (Regs regs) tok = do
  tokjson ← decrypt k tok
  (user,pass) ← J.decode $ tolazy tokjson
  pwhash ← Map.lookup user regs
  let allgood = PW.verifyPassword (Text.encodeUtf8 pass) pwhash
  if not allgood then Nothing else
    Just user

register ∷ Registrations → User → Password → IO(Maybe Registrations)
register (Regs regs) user (Password p) = do
  pwhash ← PW.makePassword p 14
  if Map.member user regs then return Nothing else
    return $ Just $ Regs $ Map.insert user pwhash regs


-- [[Utillities]]
tolazy ∷ BS.ByteString → LBS.ByteString
tolazy b = LBS.fromChunks [b]

unlazy ∷ LBS.ByteString → BS.ByteString
unlazy = BS.concat ∘ LBS.toChunks
