# Copies Functions1/lang.rkt and Functions1/lang/reader.rkt to the other 8 languages.
# This is a big hack. Test the results.

function make_lang {
    dest=$1
    lang=$2
    reduction=$3
    name=$4
    mkdir $dest
    mkdir $dest/lang
    cp Functions1/lang.rkt $dest/lang.rkt
    cp Functions1/lang/reader.rkt $dest/lang/reader.rkt
    sed -i -e "s/func-lang-1/$lang/g" $dest/lang.rkt
    sed -i -e "s/functions1/$reduction/g" $dest/lang.rkt
    sed -i -e "s/Functions1/$name/g" $dest/lang/reader.rkt
}

cd ..
rm -rf Functions2 Functions3 Records1 Records2 Records3 Variables1 Variables2 Variables3
make_lang Functions2 func-lang-2 functions2 Functions2
make_lang Functions3 func-lang-3 functions3 Functions3
make_lang Records1 record-lang-1 records1 Records1
make_lang Records2 record-lang-2 records2 Records2
make_lang Records3 record-lang-3 records3 Records3
make_lang Variables1 var-lang variables1 Variables1
make_lang Variables2 var-lang variables2 Variables2
make_lang Variables3 var-lang variables3 Variables3
