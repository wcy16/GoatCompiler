## Introduction

This program contains a parser for a programming language called Goat.

## How to compile

For compilation:

    make

For remove:

    make clean

## Usage

    Goat [option] filename
    
For the options:

    -a: output the abstract syntax tree
    -p: output the formatted source code
    -c: output the Oz machine code
    default: output the Oz machine code.

## Program structure

+ **GoatAST.hs**:  define data structures to represent abstract syntax trees (ASTs) for Goat.

+ **GoatParser.hs**: parser for Goat.

+ **GoatPrettyPrinter.hs**: pretty printer for Goat.

+ **CodeGenerate.hs**: generate Oz code for the given Goat ASTs

+ **Goat.hs**: program's main module
