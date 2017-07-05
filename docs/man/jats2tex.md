% JATS2TEX(1)
% Pedro Tacla Yamada, Beijaflor Software (https://beijaflor.io)
% June 2017

# NAME

jats2tex – Customizable JATS to LaTeX conversion.

# SYNOPSIS

**jats2tex** [**-h**] [**--template**␣*TEMPLATE_FILE*] [**--output** *OUTPUT_FILE*] [**--max-width**␣*MAX_COLUMN_WIDTH*] [**--warnings**] *INPUTFILE*

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

**-W**, **--warnings**
:   Enable outputting warnings

*INPUT_FILE*
:   Specify the input XML file to process

# OTHER COMMANDS

**jats2tex version**
:   Print the installed **jats2tex** version

**jats2tex upgrade**
:   Look for a new release of **jats2tex** and try to automatically upgrade

# CUSTOMIZING THE OUTPUT

*LaTeX* generation is implemented with a very **lightweight** *XML*
transformation language like *XLST*, which is encoded through an *YAML* template
and specified through the *-t* flag:

**jats2tex** -t *./custom-template*

The full updated documentation for the syntax and it's examples is available on
*https://github.com/beijaflor-io/jats2tex*.

## TEMPLATE SYNTAX - INTRODUCTION

Template files map `{tag-name}: "\\correspondingLaTeX"` and allows the
interpolation of **context variables** for *applying* child node transformations
at a certain point and **interpreted expressions** in the *Lua* and *Haskell*
programming languages.

## TEMPLATE SYNTAX - CONTEXT VARIABLES
- _**@@children**_ Interpolates the result of converting children of the current
  node
- _**@@heads**_ Interpolates the result of converting children of the current node
  that are marked as **head**
- _**@@bodies**_ Interpolates the result of converting children of the current
  node that are marked as **body**

## TEMPLATE SYNTAX - DEFINING BASIC TAGS
Tags are mapped to *LaTeX* with:

    conteudoxml: |
      \conteudolatex{@@children and other variables}

The output can be marked to be interpolated as *@@heads* and *@@bodies* by being
defined like this (both are optional, a tag can have *head:* or *body:* only):

    conteudoxml-com-head:
      # '@@bodies' section of this node
      body: |
        ...
      # '@@heads' section of this node
      head: |
        \title{...}

## TEMPLATE SYNTAX - LUA INTERPOLATION
Users can interpolate expressions in the *Lua* programming language that can
access and query the XML cursor information and run arbitrary transformations on
the strings its children generate and that it returns.

The syntax for interpolation is *@@lua(... lua code here ...)@@* and it'll
interpolate the *return* value of this expression where it is on the template:

    p: |
      \saida{@@lua(
        return find("font")
      )@@}

The helpers exposed to the *Lua* interpreter are:

- *children()* Like *@@children*, returns a string of the converted children
- *attr(<attr>)* Returns the value of *<attr>* in the current tag, or the empty
  string
- *find(<tag>)* Returns the result of converting children that match *<tag>*
- *elements()* Returns an array of the strings resulting from converting
  children that are elements, ignoring leaf text nodes
