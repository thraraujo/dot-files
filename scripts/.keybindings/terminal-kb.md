# Terminal: Basic Commands

Here I collect some basic terminal commands.

## Getting Started

The first commands are

```
whoami                      It prints the username
uname -a                    It prints system information
date                        It prints the date 
uptime                      Show uptime
pwd                         It shows the directory we are working in
ls                          It lists the files in the directory
```

The `ls` command has some flags we need to pay attention. Some of them are

```
-a                          Show all files, including hidden files
-r                          Reverse order
-R                          Recursive list
-t                          Sort by date of modification - more recently first
-S                          Sort by size
-l                          Long list format
-1                          One file per line
-m                          Output is separated by comma
```

## Moving around and files management

Here, let us see some important commands that help us to do some nice stuff

```
cd <Path directory>         Change directory 
mkdir <folder>              Creates the folder
touch <file>                Creates a file
cp <file> <directory>       Copy the file to the new directory
mv <file> <directory>       Moves the file. It can also be used to rename files
rm <file>                   Removes the file
rm -rf <folder>             Removes the directory and its content. -rf Recursive force. Be careful
```

One important observation is that the dot `.` denotes the directory we are currently in, whilst
the double dot `..` denotes the one-level up directory.

_For MacOS users. We can copy and paste some outputs using the `pbcopy` and `pbpaste` commands.
For example,_

```
pwd | pbcopy                Copy the working directory to clipboard
pbpaste                     Paste the clipboard to the terminal
```
