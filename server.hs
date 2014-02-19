{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.List (intersperse)
import Data.String (fromString)
import qualified Data.Text as Text
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai.Handler.Warp (run)
import Prelude.Unicode ((∘))
import qualified Network.Wai as Warp
import qualified System.Directory as Dir

fileExtension ∷ String → Maybe String
fileExtension path = r [] $ reverse path where
	r _ [] = Nothing
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
	hdr = [("content/type", fromString $ deriveContentType $ path)]

textPlain ∷ [(CI BS.ByteString, BS.ByteString)]
textPlain = [("content/type", "text/plain")]

sendCaptions ∷ Warp.Response
sendCaptions = Warp.responseFile status200 textPlain "./captions" Nothing

main :: IO()
main = do
	dirList ← Dir.getDirectoryContents "./pics"
	let picList = fromString $ unlines $ filter (not∘all(=='.')) $ dirList
	let route path = case path of
		[]            → route ["index.html"]
		["pics"]      → Warp.responseLBS status200 textPlain picList
		["captions"]  → sendCaptions
		[file]        → sendFile $ "./" ++ file
		["pics",file] → sendFile $ "./pics/" ++ file
		_             → Warp.responseLBS status404 [] ""
	run 5001 $ return ∘ route ∘ map Text.unpack ∘ Warp.pathInfo
