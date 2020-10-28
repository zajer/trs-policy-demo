DIR=$PWD/exp
if ! [ -d "$DIR" ]; then
  mkdir exp
fi
bigrapher full -d $PWD/exp/ -s -t $PWD/exp/full.svg -f svg demo.big