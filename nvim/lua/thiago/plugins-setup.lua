-- Now we use packer to configure our plugins

-- This part installs packer if it is not installed
local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
		vim.cmd([[packadd packer.nvim]])
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

-- Reloads neovim whenever you save this file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- This is protected call
local status, packer = pcall(require, "packer")
if not status then
	return
end

-- Now we can add some packages
return packer.startup(function(use)
	use("wbthomason/packer.nvim") -- packer itself
	use("christoomey/vim-tmux-navigator") -- tmux & split window navigation
	use("szw/vim-maximizer") -- maximizes and restores current window

  -- Colorchemes
  -- use("arcticicestudio/nord-vim") -- nord colorsheme - i think it is not very good
  -- use('shaunsingh/nord.nvim') -- nord colorscheme 
  use("EdenEast/nightfox.nvim") -- nightfox

  -- Dashboard - Startify
  -- use('mhinz/vim-startify')

  -- essential plugins
  use("tpope/vim-surround") -- add, delete, change surroundings (it's awesome)
  -- use("inkarkat/vim-ReplaceWithRegister") -- replace with register contents using motion (gr + motion)

  -- commenting with gc
  use("numToStr/Comment.nvim")


  -- file explorer
  use({
  	"nvim-tree/nvim-tree.lua",
  	requires = {
  	"nvim-tree/nvim-web-devicons", -- optional, for file icons
  	},
  	tag = "nightly", -- optional, updated every week. (see issue #1193)
  })
  
  -- vs-code like icons
  use("nvim-tree/nvim-web-devicons")
  
  -- statusline
  use("nvim-lualine/lualine.nvim")
  
  -- fuzzy finding w/ telescope
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }) -- dependency for better sorting performance
  use({
  	"nvim-telescope/telescope.nvim",
  	branch = "0.1.x",
  	requires = { { "nvim-lua/plenary.nvim" } },
  })

  -- lsp zero
  use({
  'VonHeikemen/lsp-zero.nvim',
    requires = {
    -- LSP Support
    {'neovim/nvim-lspconfig'},
    {'williamboman/mason.nvim'},
    {'williamboman/mason-lspconfig.nvim'},

    -- Autocompletion
    {'hrsh7th/nvim-cmp'},
    {'hrsh7th/cmp-buffer'},
    {'hrsh7th/cmp-path'},
    {'saadparwaiz1/cmp_luasnip'},
    {'hrsh7th/cmp-nvim-lsp'},
    {'hrsh7th/cmp-nvim-lua'},

    -- Snippets
    {'L3MON4D3/LuaSnip'},
    -- Snippet Collection (Optional)
    {'rafamadriz/friendly-snippets'},
  }
  })

  -- treesitter configuration
  use({
  	"nvim-treesitter/nvim-treesitter",
  	run = function()
  		local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
  		ts_update()
  	end,
  })
  
  -- auto closing
  -- use("windwp/nvim-autopairs") -- autoclose parens, brackets, quotes, etc...
  use({"windwp/nvim-ts-autotag", after = "nvim-treesitter"}) -- autoclose tags. This seems to be a html plugin
  
  -- git integration
  use("lewis6991/gitsigns.nvim") -- show line modifications on left hand sidek
  
  if packer_bootstrap then
  	require("packer").sync()
  end
end)
