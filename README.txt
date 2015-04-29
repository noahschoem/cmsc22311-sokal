Project: Lab #1: Sokal
Author:  Noah Schoem
For:    CMSC 22311, Spring 2015

Description: My implementation of the Sokal Lab.
The source code for this project is available at https://github.com/noahschoem/cmsc22311-sokal
and this code is licensed under public domain.

Usage: 
sokal_suck: Simply invoke on the command line and sokal_suck will build 
a model at sokal.model in the current working directory.

sokal_spew: Simply invoke on the command line and sokal_spew will spew 
text using the model at sokal.model in the current working directory.  Flags are:

  -l m  --length=m   desired minumum length of spewed text.  Default is 100.
  -s n  --seed=n     for the user to supply their own seed for the random number generator, if so desired
  -o    --show-seed  show the value of the generator seed.  Default is suppressing the generator seed.
  -h    --help       show this help text

