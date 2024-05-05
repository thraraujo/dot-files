# Vim Cheatsheet: Basics

Let us start with the notation:

```
C-a = Ctrl-a
```

## Getting Started
```
vim <file>                  Open file.
vim <file1> <file2>         Open file1 and file2
vim -R <file>               Open file in read mode
view <file>                 Open file in read mode
:h[elp] <keyword>           Open help for the keyword
:term                       Open a terminal
:! <cmd>                    Execute shell command <cmd>
<C-\><C-n>                  Go to command mode in the built-in terminal
ESC                         Exit insert mode
```

```
:q                          Close document if no modification has been done
:w                          Save the document
:wa                         Save all windows
:wq                         Save and close
:x                          Save and close, equivalent to wq
:q!                         Close and ignore changes
```

Other fundamental commads are the following

```
:set spell                  Start the spell checking
:set cc=100                 Creates a vertical line at character 100. Evidently we can change the value 100 to other value. Moreover, cc stands for colorcolumn
```

## Navigation

* _One line_

```
h                           Left
j                           Down
k                           Up
l                           Right
```

If we consider the notation _nh_, where _n_ is a number known as _replication factor_, the left
motion is repeated _n_ times. As an example, we have the equivalence: _4h = hhhh_. The same is
true for _j_, _k_ and _l_.

```
0                           Move cursor to the beggining of the line
$                           Move cursor to the end of the line
^                           Move cursor to the first non-blank character
```

* _Text blocks_

```
w                           Move forward one word
W                           Move forward one word (ignore punctuation)
b                           Move backward one word
B                           Move backward one word (ignore punctuation)
e                           Move forward to the end of the word
E                           Move forward to the end of the word (ignore punctuation)
gg                          Move the first line of the document
G                           Move the last line of the document
```

If we write _nG_, where _n_, the cursor goes to line _n_. The same is true for the other motions.
Then, 1G is equivalent to gg.

* _Large movements_

```
H                           Go to the first character on the top
M                           Go to the first character on the middle
L                           Go to the first character on the bottom
C-F                         Scroll forward one page
C-B                         Scroll backward one page
C-D                         Scroll forward half page
C-U                         Scroll backward half page
```

* _Moving around sentences, paragraphs and sections_

```
(                           Go to the beginning of the sentence
)                           Go to the beginning of the next sentence
{                           Go to the beginning of the paragraph
}                           Go to the beginning of the next paragraph
[[                          Go to the beginning of the section
]]                          Go to the beginning of the next section
```

## Edition

* _Inserting text_

```
i                           Insert - the insert cursor goes to the beginning of the block cursor
I                           Insert - the insert cursor goes to the beginning of the line
0i                          Insert - the insert cursor goes to the beginning of the line.
a                           Append - the insert cursor goes to the end of the block cursor
A                           Append - the insert cursor goes to the end of the line
$a                          Append - the insert cursor goes to the end of the line
o                           Text line after the cursor
O                           Text line before the cursor
```

* _Changing text_

```
cw                          Change word
c$                          Change text to the end of the line
C                           Change text to the end of the line. Equivalent to c$
c0                          Change text starting from the beggining of the line
cc                          Change the entire line
```

* _Replacing text_

```
r                           Replace a single character
R                           Replace characters. Overstrike.
s                           Substitute character. Similar to r but enters in insert mode
S                           Substitute the whole line. Similar to C
~                           Change case: lowercase to uppercase and vice-versa
```

* _Delete text_

```
dw                          Delete a word starting from the cursor position
dd                          Delete the line
D                           Delete the line. Equivalent to dd
d0                          Delete from the cursor position to the beggining of the line
d$                          Delete from the cursor position to the end of the line
x                           Delete a single character under the cursor
X                           Delete a single character in front of the cursor
```

* _Undoing, repeating and joining_

```
u                           Undo recent modification
C-r                         Redo
U                           Retores the line
.                           Repeat the last command
J                           Join the cursor line with the line below it
```

When we delete a character, word or line, vim saves the deleted object in a temporary register,
so we can recover this deletion as long as we do not delete any other object. We have seen that we
undo this deletion with the command _u_. It also true that we can paste the deleted object in a
new place. Let us take a look on these commands.

## Copy and Paste

```
yw                          Yank (Copy) word
yy                          Yank the whole line
Y                           Yank the whole line. Equivalent to yy
y$                          Yank to the end of the line
y0                          Yank to the beginning of the line
p                           Paste after the cursor
P                           Paste before the cursor
"+y                         Yank into the system clipboard
"+p                         Paste from the system clipboard
```

## Search

```
/pattern                    Search pattern forward
?pattern                    Search pattern backward
n                           Repeat search in the same direction
N                           Repeat search in the opposite direction
/                           Repeat search forward
?                           Repeat search backward
```