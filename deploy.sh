# This script builds an executable to be uploaded to the production server. We
# assume that this is run on a fresh (Ubuntu Server 13.10) installation, and
# that we are uploading to another server running the same distribution.

set -e
uploadto=$1
echo $uploadto
test -n "$deploy" || {
    echo -n "Where should we scp the files? ";
    read uploadto; }

sudo apt-get update
sudo apt-get upgrade
sudo apt-get install haskell-platform git realpath

cabal update
cabal install cabal-install
export PATH="$HOME/.cabal/bin:$PATH"

git clone https://github.com/bsummer4/habits.git
cd habits
cabal sandbox init
cabal install --only-dependencies
cabal build --ghc-options='-O3 -Wall -static'

deploy=$(mktemp -dp.)
deploy=$(realpath $deploy)
trap "rm -rf $deploy" EXIT

cp dist/build/habits/habits $deploy/habits
cd $deploy
openssl aes-256-cbc -d <../secret.tar.xz.aes | unxz | tar x
cp ../index.html .
scp * $uploadto
