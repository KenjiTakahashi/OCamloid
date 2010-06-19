usage="Usage:\nmake <option>. Available options:\ncompile - to compile the source into binary code.\nclean - Remove all files made in compilation."
if [ -n "$1" ]
then
    if [ "$1" = "compile" ]
    then
        ocamlopt -c io.ml
        ocamlopt -c menu.ml
        ocamlopt -thread -c game.ml
        ocamlopt -thread -o ocamloid unix.cmxa graphics.cmxa threads.cmxa str.cmxa io.cmx menu.cmx game.cmx main.ml 
        echo -e "\nCompiled into binary named "ocamloid"."
    elif [ "$1" = "clean" ]
    then
        rm *.o
        rm *.cm*
        rm ocamloid
        echo -e "Cleaned. You can recompile now."
    else
        echo -e "$usage"
    fi
else
    echo -e "$usage"
fi

