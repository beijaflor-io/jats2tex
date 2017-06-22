% JATS2TEX(1)
% Pedro Tacla Yamada, Beijaflor Software (https://beijaflor.io)
% June 2017

# NAME

jats2tex – Customizable JATS to LaTeX conversion.

# SYNOPSIS

**jats2tex** [**-h**] [**--template**␣*TEMPLATE_FILE*] [**--max-width**␣*MAX_COLUMN_WIDTH*] [**--output** *OUTPUT_FILE*] *INPUTFILE*

# DESCRIPTION

**jats2tex** converts JATS-XML into LaTeX by using a YAML template mapping
between XML tags and LaTeX strings with a lightweight interpolation syntax and
an embedded Haskell interpreter for arbitrary manipulation when converting.

# GENERAL OPTIONS

**-h**, **--help**
:   Display a help text

**-t**, **--template** *TEMPLATE_FILE*
:   Specify a custom YAML template

**-w**, **--max-width** *MAX_COLUMN_WIDTH*
:   Specify max line length by columns or 0 to disable wrapping

**-o**, **--output** *OUTPUT_FILE*
:   Specify the output file, prints to standard output if not provided

*INPUT_FILE*
:   Specify the input XML file to process

# OTHER COMMANDS

**jats2tex version**
:   Print the installed **jats2tex** version

**jats2tex upgrade**
:   Look for a new release of **jats2tex** and try to automatically upgrade
