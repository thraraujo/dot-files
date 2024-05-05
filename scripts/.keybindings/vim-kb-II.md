# Vim Cheatsheet: Beyond the Basics

## Registers

When we delete something with vim, the previous 9 deletions are saved in an unnamed register.
We can access these registers with numbers from 1 to 9. For example:

```
"nP                         Paste the register n, where n runs from 1 to 9, before the cursor
"np                         Paste the register n, where n runs from 1 to 9, after the cursor
```

On the other hand, one can use named registers. Vim has 26 of them: The letters from a to z.
For example, using the register t, we have

```
"tyy                        Yank the current line to the register labeled by t
"t5yy                       Yank the next five lines to the register labeled by t
"tP                         Paste the content of the register t before the cursor
"tp                         Paste the content of the register t after the cursor
```

One can also delete and save the deletion to a register as

```
"tdd                        Delete the line and save the content to the registered t
```

## Marks

One cal also mark parts of the text with the following commands:

```
m a                         Mark current position with a. 
```

_a_ can be any letter, and vim distinguishes uppercases and lowercases.
One can move to the marked positions with

```
'a                          Move cursor to the line marked with a 
`a                          Move cursor to the character marked with a
``                          Return to the exact position of the previous mark after a move. 
''                          Return to the beginning of the line of the previous mark or context
```

## Folding

```
zfnj                        Create folding to the nj position. We can mark a position with the previous command
zo                          Open folding
zc                          Close folding
```

## The Ex editor

The ex editor is behind the curtains of vim, so it might be useful to learn one or two things
about it. First, when we use the colons ':', we are using the ex editor. So, it is an _line_
editor. Some important commands are the following:

```
:n                          Cursor goes to line n
:nd                         Delete line n
:n1,n2d                     Delete lines n1 to n2
:ny                         Copy line n
:n1,n2y                     Copy lines n1 to n2
:n1mn2                      Move line n1 to the line below n2
:n1,n2mn3                   Move lines n1 to n2 the line below n3 (It is like delete and paste)
```

Symbols with special meanings

```
:.                          Represents the current line
:%                          Represents the entire document
:p                          Print the current line 
:#                          Print the current line with the number
```

Other useful commands are

```
:=                          Print the total number of lines in the document 
:.=                         Print the number of the current line
```

It is important to notice that we have some special symbols. For example, we already know that
$ means the end of the final line, and 0 means the beginning of the current line. But in the ex
command, these symbols mean different things. For example

```
:0                          Cursos goes to the beginning of the document. It is equivalent to gg
:$                          Cursos goes to the end of the document. It is equivalent to G 
```

So, we can combine these symbols with commands. For example, one can delete the current line,
through the end of the document with _:.,$d_

We already know how to make searches with vim, but ex also has a global search pattern. That means
that we can search for a pattern and the editor shows all lines that contain the searched pattern.

```
:g/pattern                  Shows all the lines that contain the pattern
:g!/pattern                 Shows all the lines that do not contain the pattern
```

## Working with multiple files

We have already seen how to open more than one file, but we have not explored this possibility yet.

```
vim <file1> <file2>         Open file1 and file2
:e <file>                   Open the file from the vim editor (e means edit)
:bn                         Move to the next files(buffer next)
:bp                         Move to the previous files(buffer previous)
:bd                         Close file (buffer delete)
```

## Global replacement

Now, we would like to search and replace patterns.

```
:s/old/new                  This is the search command. It changes the first occurrence. 
:s/old/new/g                It changes all the occurrences in the line
:%s/old/new/g                It changes all the occurrences in the document
:m,ns/old/new/g             It changes all the occurrences between lines m to n.
```

Sometimes we want to search for patterns with special characters. For example, dots, question marks
and slashes. We can avoid these symbols with another slash. For example \\ means \. This is
particularly convenient when we are writing LaTeX documents.
