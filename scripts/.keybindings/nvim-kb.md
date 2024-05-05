-------------------

1. GENERAL KEYMAPS

-------------------

<leader>: <SPC>

jk: <ESC> exit insert mode <ESC>

x: delete single character without copying into register

<leader>nh: clear search highlights

<leader>+: increase numbers
<leader>-: decrease numbers

<leader>sv: <C-w>v split window vertically
<leader>sh: <C-w>s split window horizontally
<leader>se: <C-w>= make split windows equal width & heigh
<leader>sx: close current split

<leader>to: open new tab
<leader>tx: close current tab
<leader>tn: go to next tab
<leader>tp: go to previous tab

-------------------

2. PLUGIN KEYBINDS

-------------------

 <leader>sm: toggle split window maximization

 <leader>e: toggle file explorer

 <leader>ff: find files within current working directory, respects .gitignore
 <leader>fs: find string in current working directory as you type
 <leader>fc: find string under cursor in current working directory
 <leader>fb: list open buffers in current neovim instance
 <leader>fh: list available help tags

 <leader>gc: list all git commits (use <cr> to checkout) ["gc" for git commits]
 <leader>gfc: list git commits for current file/buffer (use <cr> to checkout) ["gfc" for git file commits]
 <leader>gb: list git branches (use <cr> to checkout) ["gb" for git branch]
 <leader>gs: list current changes per file with diff preview ["gs" for git status]

 <leader>rs: restart lsp server

 csw " : surround the word with the symbol "
 cs "  } : change the symbol " to }
 ds "  : remove the symbol "
