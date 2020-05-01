set -e
find ./src/ -name *.purs -exec purty --write {} \;
find ./test/ -name *.purs -exec purty --write {} \;
find ./ -name '*.dhall' -exec dhall format --inplace {} \;
