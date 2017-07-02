tablize(1) -- pretty-print CSV file 
===================================

## SYNOPSIS
`tablize` [`-x` | `--horizontal` _HDECOR_] [`-y` | `--vertical` _VDECOR_]
[`-a` | `--alignment` _ALIGNS_] [_FILE_]

## DESCRIPTION
The `tablize` utility pretty-prints tables defined by the CSV format (the
parsing of the format is RFC4180 complaint). It provides column alignments and
visual decoration of the table both horizontally and vertically. A
simple domain-specific language was introduced with the purpose of describing
the table decorations. Its grammar is introduced in the `DECORATION LANGUAGE`
section of this manual page.

## OPTIONS
The only positional argument of the program is the path to the CSV file. If
omitted, standard input stream is used instead. Optional arguments are:

 * `-x | --horizontal`:
   Defines the horizontal decoration, using the custom DSL described below
   (default: `union(outer, only(1))`).

 * `-y | --vertical`:
   Defines the vertical decoration, using the custom DSL described below
   (default: `all`).

 * `-a | --alignment`:
   Defines the column alignment starting from the left-most column (default:
   empty string). Valid alignments are `left`, `centre` and `right`. A valid
   definition is a comma-separated list of these alignments. Alignments are
   assigned starting from the left, while all unspecified columns will
   default to value `left`.
   
## DECORATION LANGUAGE
The decoration language  is used to define the presence of decorative lines
(separately for horizontal and vertical) within the table. Lines can be used
to separate sections of the table - always the full length, row or column.

The decoration is represented by exactly one statement. All statements are
described below:

The `all` statement results in all lines (in the relevant orientation) to be
visible. Opposite effect can be achieved via the `none` statement that,
naturally, makes no decorative lines visible.

The `inner` and `outer` statements, as the names suggest, provide means of
decorating the inner and outer lines of the table respectively. These
statements are best used when the number of columns or rows is not known
beforehand.

The `only` statement expects a list of zero or more unsigned integer indices
separated by the comma character(leftmost and topmost lines equal to zero)
that should be contained in the decoration. An example of the statement is:
`only(1, 2, 3)`. The opposite statement, `except`, includes all but
lines specified with indices.
 
Two combinators - `union` and `isect` can be used to specify a set union and
set intersection of zero or more decorations separated by the comma character.
The default value for the horizontal decoration is `union(outer, only(1))`,
resulting in decoration seen in the Example section.

All whitespace is ignored and can be used for stylistic purposes.

## AUTHOR
Daniel Lovasko `<daniel.lovasko@gmail.com>`
