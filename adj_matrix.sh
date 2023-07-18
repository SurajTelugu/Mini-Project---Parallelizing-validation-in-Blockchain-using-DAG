eval $(opam env --switch=5.0.0)
ocamlfind ocamlc  -g -thread -package unix,lockfree -linkpkg  core_adj_matrix.ml -o core_adj_matrix
rm core_adj_matrix.cmi core_adj_matrix.cmo
./core_adj_matrix $1