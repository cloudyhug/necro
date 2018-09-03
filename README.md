# Necro : making skeletons alive !

---

**Necro** is an OCaml project born at **Inria Rennes** in June 2018, in relation to a research article (*submitting in progress*) I discovered during my internship there, from June to August.

The goal of the project is to generate an interpreter for a programming language, given the semantics of this language as an input file, written in a particular format, and an OCaml module describing the memory representation of the language's basic types.

---

### Dependencies

* OCaml
* make

### How to run it

Three rules (`make`, `make test` and `make clean`) are available for the user. The project is compiled as one main program. Check **MORE.md** to get more information about the inputs to provide and the structure of the output interpreter.