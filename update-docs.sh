set -ex

git checkout master
ROOT=`stack path --local-doc-root`
stack haddock
git checkout gh-pages
git rm -rf *
cp -r $ROOT/* .
git add *
git commit -m "docs"
git push
git checkout master
