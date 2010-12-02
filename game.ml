ocamlopt -c menu.ml
ocamlopt -thread -c game.ml
ocamlopt -thread -o ocamloid.exe unix.cmxa graphics.cmxa threads.cmxa menu.cmx game.cmx -cclib -lthreads main.ml 

