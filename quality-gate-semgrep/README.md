# Why semantic grepping?

# How to build and use

To run locally:
[Build OCaml grammar](https://ast-grep.github.io/advanced/custom-language.html)
```
ln -s ~/tree-sitter-ocaml/grammars/ocaml/libtree-sitter-ocaml.so ocaml.so
sg scan ocaml/ -c quality-gate-semgrep/sgconfig.yml --color=always
```

# How to add rules

Write a short OCaml snippet in `shortrules`, run `python autogen.py`.

# How to expand rules
