

# Overview

This is project is a Haskell reimplementation of the ELIZA Program as described by Joseph Weizenbaum in Volume 9 of the January, 1966 edition of the Communications of the ACM.

In summary, the project includes:

- An interpreter for ELIZA scripts
- An interactive program that simulates human conversation based off a loaded script

# ELIZA

ELIZA is a program operating within the MAC time-sharing system at MIT which makes  certain kinds of natural language conversation between man and computer possible.  Input sentences are analyzed on the basis of decomposition rules which are  triggered by key words appearing in the input text. Responses are generated by  reassembly rules associated with selected decomposition rules. The fundamental  technical problems with which ELIZA is concerned are:

(1)  the  identification  of key  words,
(2)  the  discovery  of  minimal  context,
(3)  the  choice of  appropriate  transformations,
(4)  generation  of  responses  in the  absence  of  key  words,  and
(5)  the  provision  of  an  editing capability  for  ELIZA "scripts".

# Setup

To run the program, you need to compile ELIZA and then give it a script to use for a conversation. For instance.

```
ghc eliza.hs
./eliza eliza.e
```
