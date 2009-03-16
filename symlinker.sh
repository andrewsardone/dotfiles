DIR="$PWD"
pushd $DIR
for f in *; do
    pushd ~
    ln -s "$DIR/$f" ".$f"
    popd
done
popd