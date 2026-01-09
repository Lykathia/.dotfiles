return {
	-- UI / Movement
	-- { "vim-airline/vim-airline" }, should be already replaced with lualine
	{ "easymotion/vim-easymotion" },
	{ "folke/noice.nvim", enabled = false },
	{
		"LazyVim/LazyVim",
		opts = {
			colorscheme = "default",
		},
	},
	{ "nvim-mini/mini.pairs", enabled = false },

	-- Git
	{ "tpope/vim-fugitive" },

	-- Diagnostics / linting
	-- { "dense-analysis/ale" }, not needed I think

	-- Fuzzy finding
	{ "junegunn/fzf" },
	{ "junegunn/fzf.vim" },

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"lua",
				"javascript",
				"typescript",
				"python",
				"html",
				"css",
				"svelte",
				"go",
				"bash",
				"yaml",
				"json",
				"gomod",
			},
			highlight = { enable = true },
			indent = { enable = true },
		},
		build = ":TSUpdate",
	},

	-- Svelte plugin
	{ "leafOfTree/vim-svelte-plugin" },

	-- LSP Config
	{
		"mason-org/mason-lspconfig.nvim",
		opts = {
			ensure_installed = { "lua_ls", "gopls" },
		},
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			"neovim/nvim-lspconfig",
		},
	},
}
