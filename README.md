# tablize
Pretty-printing of CSV files.

## Introduction
The `tablize` utility can be used to pretty-print tables defined by the
CSV format. It provides column alignments and visual decoration of the table
both horizontally and vertically. To do so, a simple domain-specific language
was introduced with its grammar described in the Decoration language section of
this document.

## Example
A simple table defining services and network ports for different hosts in the
network:
```
$ cat config.csv
host,service,port
qa.example.com,apache,443
testing.example.com,apache,8080
*.example.com,apache,80
```

The above table can be rendered into the following text:
```
$ tablize config.csv -x 'union(outer, only(1))' -y all -a left,left,right
+---------------------+---------+------+
| host                | service | port |
+---------------------+---------+------+
| qa.example.com      | apache  |  443 |
| testing.example.com | apache  | 8080 |
| *.example.com       | apache  |   80 |
+---------------------+---------+------+
```

## Build & install
There are two standard ways of obtaining the utility:
 * by cloning the git repository: `git clone https://github.com/lovasko/tablize`
 * by using the central Hackage server: `cabal install tablize`

### Dependencies
The build process depends on the following packages from Hackage:
 * `base`
 * `attoparsec`
 * `comma`
 * `optparse-applicative`
 * `tabl`
 * `text`

## Options
The command-line interface consists of an optional positional argument that
specifies the input file (`stdin` is used if no file is specified) and three
optional named parameters:

 * `-x|--horizontal HDECOR` defines the horizontal decoration, using the custom
   DSL described below (default: `union(outer, only(1))`)

 * `-y|--vertical VDECOR` defines the vertical decoration, using the custom
   DSL described below (default: `all`)

 * `-a|--alignment ALIGN` defines the column alignment starting from the
   left-most column (default: empty string). Valid alignments are `left`,
   `centre` and `right`. Alignments are assigned starting from the left side,
   while all unspecified columns will default to `left`.


## Decoration language
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

All whitespace is disregarded as non-relevant and can be used for stylistic
purposes.

### Examples
The following set of examples with operate on a simple 3x3 table filled with
`o` characters:
```
$ cat o.csv
o,o,o
o,o,o
o,o,o
```

#### Inner lines only
```
$ tablize o.csv -x inner -y inner
o | o | o
--+---+--
o | o | o
--+---+--
o | o | o
```

#### Outer lines only
```
$ tablize o.csv -x outer -y outer
+-------+
| o o o |
| o o o |
| o o o |
+-------+
```

#### Separating the first column
```
$ tablize o.csv -x none -y 'only(1)'
o | o o
o | o o
o | o o
```

#### Default settings
```
$ tablize o.csv -x 'union(outer, only(1))' -y all
+---+---+---+
| o | o | o |
+---+---+---+
| o | o | o |
| o | o | o |
+---+---+---+
```

#### Separating all rows
```
$ tablize o.csv -x all -y none
-----
o o o
-----
o o o
-----
o o o
-----
```

## License
The source code of the `tablize` utility is licensed under the terms of the
[2-clause BSD license](LICENSE). Feel free to contact the author if you need a
different license for your particular use-case.

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
