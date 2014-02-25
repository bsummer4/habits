{-# LANGUAGE OverloadedStrings, UnicodeSyntax, QuasiQuotes #-}

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.List (intersperse)
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai.Handler.Warp (run)
import Prelude.Unicode ((∘))
import qualified Network.Wai as Warp
import qualified System.Directory as Dir

import Text.Hamlet (shamlet)
import Text.Julius (Javascript, JavascriptUrl, js, renderJavascript, rawJS)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)

fileExtension ∷ String → Maybe String
fileExtension path = r [] $ reverse path where
	r _ [] = Nothing
	r acc ('.':_) = Just acc
	r acc (c:cs) = r (c:acc) cs

deriveContentType ∷ String → String
deriveContentType path = case fileExtension path of
	Just "html" → "text/html"
	Just "js"   → "application/javascript"
	Just "jpg"  → "image/jpeg"
	Just "jpeg" → "image/jpeg"
	Just "webp" → "image/webp"
	Just "gif"  → "image/gif"
	_           → "text/plain"

-- TODO 404 if a file doesn't exist.
sendFile ∷ String → Warp.Response
sendFile path = Warp.responseFile status200 hdr path Nothing where
	hdr ∷ [(CI BS.ByteString, BS.ByteString)]
	hdr = [("Content-Type", fromString $ deriveContentType $ path)]

textPlain ∷ [(CI BS.ByteString, BS.ByteString)]
textPlain = [("Content-Type", "text/plain")]
textHtml ∷ [(CI BS.ByteString, BS.ByteString)]
textHtml = [("Content-Type", "text/html")]
ctyJs ∷ [(CI BS.ByteString, BS.ByteString)]
ctyJs = [("Content-Type", "application/javascript")]

sendCaptions ∷ Warp.Response
sendCaptions = Warp.responseFile status200 textPlain "./captions" Nothing

ls ∷ String → IO [String]
ls path = Dir.getDirectoryContents path >>= return∘(filter $ not∘all(=='.'))

main ∷ IO()
main = do
	dirList ← Dir.getDirectoryContents "./pics"
	let clientJSStr = clientJS$ filter (not∘all(=='.')) $ dirList
	let indexHtml = client
	let route path = case path of
		[]            → Warp.responseLBS status200 textHtml$fromString indexHtml
		["UI.js"]     → Warp.responseLBS status200 ctyJs$fromString clientJSStr
		["captions"]  → sendCaptions
		[file]        → sendFile $ "./" ++ file
		["pics",file] → sendFile $ "./pics/" ++ file
		_             → Warp.responseLBS status404 [] ""
	run 5003 $ return ∘ route ∘ map Text.unpack ∘ Warp.pathInfo

jsList ∷ [String] → String
jsList strs = "[" ++ elements ++ "]" where
	elements = concat $ intersperse "," $ map (\s → "\"" ++ s ++ "\"") $ strs

fuckyesod ∷ JavascriptUrl () -> Javascript
fuckyesod jsu = jsu (\a b → "")

clientJS ∷ [String] → String
clientJS picUrls = LText.unpack wtf where
	wtf = renderJavascript j
	j ∷ Javascript
	j = fuckyesod [js|
 var lines = function(str) {
  var r = str.split("\n");
  if ((r[r.length-1]).length == 0) { r.pop(); }
  return r; };
 var shuffle = function (o) {
  for (var j, x, i = o.length ; i;) {
    j=Math.floor(Math.random() * i);
   x=o[--i]; o[i]=o[j]; o[j]=x; }
  return o; };
 picList = shuffle(#{rawJS(jsList picUrls)})
 for (var i in picList) { console.log(picList[i]); }
 var i = 0;
 var setupKeys = null;
 var next = null;
 var slideshow = false;
 next = function(diff) {
  i = i + diff;
  if (picList.length==0) return;
  if (i >= picList.length) { i=0; }
  if (i < 0) { i=picList.length-1; }
  var imgNode = document.getElementById("theimg");
  imgNode.src= "/pics/" + picList[i];
  setupKeys(); };
 setupKeys = function() {
  document.getElementById("theimg").onclick = (function(event) { next(1); })
  document.onkeypress = (function(event) {
   if (event.which == 0) return;
   char = String.fromCharCode(event.which);
   switch (char) {
    case 'l': case 'L': next(1); break;
    case 'h': case 'H': next(-1); break;
    case ' ': if (slideshow) {slideshow=false;} else {slideshow=true;};
    defeault: break; };});};
 next(0);
 window.setInterval(function(){ if (slideshow) { next(1); }}, 5000);
 |]

client ∷ String
client = renderHtml [shamlet|
$doctype 5
<html>
 <head>
  <style>
   img {
   max-width: 100%;
    height: 100%;
    position: fixed;
    left: 0; }
    top: 0; }
 <body>
  <p><img id="theimg"></img>
  <script src="/UI.js">
|]
