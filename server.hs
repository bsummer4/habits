{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

import qualified Network.Wai as Warp
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.List (intersperse)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import Prelude.Unicode

bsPack = Data.ByteString.Char8.pack
bsUnpack = Data.ByteString.Char8.unpack
lbsPack = Data.ByteString.Lazy.Char8.pack
lbsUnpack = Data.ByteString.Lazy.Char8.unpack

fileExtension ∷ String → Maybe String
fileExtension path = r [] $ reverse path where
	r acc [] = Nothing
	r acc ('.':_) = Just $ reverse acc
	r acc (c:cs) = r (c:acc) cs

deriveContentType ∷ String → String
deriveContentType path = case fileExtension path of
	Just "html" → "text/html"
	Just "js" → "application/javascript"
	Just "jpg" → "image/jpeg"
	Just "jpeg" → "image/jpeg"
	Just "webp" → "image/webp"
	Just "gif" → "image/gif"
	_ → "text/plain"

-- TODO 404 if a file doesn't exist.
sendFile ∷ String → Warp.Response
sendFile path = Warp.responseFile status200 hdr path Nothing where
	hdr ∷ [(CI BS.ByteString, BS.ByteString)]
	hdr = [("content/type", bsPack $ deriveContentType $ path)]

sendCaptions ∷ Warp.Response
sendCaptions = Warp.responseFile status200 textPlain "./captions" Nothing

main = do
	dirList ← Dir.getDirectoryContents "./pics"
	let picList = lbsPack $ unlines $ filter (not∘all(=='.')) $ dirList
	let textPlain = [("content/type", "text/plain")]
	let route path = case path of
		[]            → route ["index.html"]
		["pics"]      → Warp.responseLBS status200 textPlain picList
		["captions"]  → sendCaptions
		[file]        → sendFile $ "./" ++ file
		["pics",file] → sendFile $ "./pics/" ++ file
		_             → Warp.responseLBS status404 [] ""
	run 5001 $ return ∘ route ∘ map Text.unpack ∘ Warp.pathInfo
