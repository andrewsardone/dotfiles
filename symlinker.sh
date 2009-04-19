DIR="$PWD"
pushd $DIR
for f in *; do
	if [ $f != "symlinker.sh" -a $f != "README.md" ]; then
		pushd ~
		ln -s "$DIR/$f" ".$f"
		popd
	fi
done
popd