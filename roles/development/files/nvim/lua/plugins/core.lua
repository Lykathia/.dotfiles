return {
	-- UI / Movement
	{
		"wtfox/jellybeans.nvim",
		lazy = false,
		priority = 1000,
	},
	{ "easymotion/vim-easymotion" },
	{ "folke/noice.nvim", enabled = false },
	{
		"LazyVim/LazyVim",
		opts = {
			colorscheme = "jellybeans",
		},
	},
	{ "nvim-mini/mini.pairs", enabled = false },

	-- Coding
	{
		"nvim-mini/mini.nvim",
		version = "*",
		config = function()
			require("mini.surround").setup()
			require("mini.align").setup()
			require("mini.comment").setup()
		end,
	},

	-- Git
	{ "tpope/vim-fugitive" },
	{
		"lykathia/gitlink.nvim",
		dev = true,
		event = "VeryLazy",
	},
	{
		"2kabhishek/co-author.nvim",
		dependencies = {
			"folke/snacks.nvim",
		},
		CMD = { "COAUTHOR" },
	},

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
				"bash",
				"css",
				"go",
				"gomod",
				"html",
				"javascript",
				"json",
				"lua",
				"python",
				"svelte",
				"typescript",
				"yaml",
				"rust",
				"tsx",
				"vim",
			},
			auto_install = true,
			highlight = { enable = true },
			indent = { enable = true },
		},
		build = ":TSUpdate",
	},

	-- Colors!
	{
		"brenoprata10/nvim-highlight-colors",
		event = "User BaseFile",
		cmd = { "HighlightColors" },
		opts = { enabled_named_colors = false },
	},

	-- Svelte plugin
	{ "leafOfTree/vim-svelte-plugin" },

	-- LSP Config
	{
		"mason-org/mason-lspconfig.nvim",
		opts = {
			ensure_installed = { "lua_ls", "gopls", "rust_analyzer" },
		},
		dependencies = {
			{ "mason-org/mason.nvim", opts = {} },
			"neovim/nvim-lspconfig",
		},
	},
	-- Auto completion
	{
		"hrsh7th/nvim-cmp",
		event = "InsertEnter",
		dependencies = {
			{ "hrsh7th/cmp-nvim-lsp" },
			{ "saadparwaiz1/cmp_luasnip" },
			{ "hrsh7th/cmp-buffer" },
			{ "hrsh7th/cmp-path" },
		},
		config = function()
			local cmp = require("cmp")
			local luasnip = require("luasnip")

			-- Load friendly-snippets
			require("luasnip.loaders.from_vscode").lazy_load()

			cmp.setup({
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert({
					["<C-k>"] = cmp.mapping.select_prev_item(),
					["<C-j>"] = cmp.mapping.select_next_item(),
					["<C-Space>"] = cmp.mapping.complete(),
					["<CR>"] = cmp.mapping.confirm({ select = true }),
					["<Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
					end, { "i", "s" }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" }, -- LSP completions
					{ name = "luasnip" }, -- Snippets
					{ name = "buffer" }, -- Text in buffer
					{ name = "path" }, -- File paths
				}),
			})
		end,
	},

	-- Snippets
	{
		"L3MON4D3/LuaSnip",
		dependencies = {
			"rafamadriz/friendly-snippets",
			--boot"zeioth/NormalSnippets",
			--"benfowler/telescope-luasnip.nvim",
		},
		event = "User BaseFile",
		opts = {
			history = true,
			delete_check_events = "TextChanged",
			region_check_events = "CursorMoved",
		},
		config = function(_, opts)
			if opts then
				require("luasnip").config.setup(opts)
			end
			vim.tbl_map(function(type)
				require("luasnip.loaders.from_" .. type).lazy_load()
			end, { "vscode", "snipmate", "lua" })
			-- friendly-snippets - enable standardized comments snippets
			require("luasnip").filetype_extend("typescript", { "tsdoc" })
			require("luasnip").filetype_extend("javascript", { "jsdoc" })
			require("luasnip").filetype_extend("lua", { "luadoc" })
			require("luasnip").filetype_extend("python", { "pydoc" })
			require("luasnip").filetype_extend("rust", { "rustdoc" })
			require("luasnip").filetype_extend("sh", { "shelldoc" })
		end,
	},

	-- Testing
	{
		"nvim-neotest/neotest",
		dependencies = {
			"nvim-neotest/nvim-nio",
			"nvim-lua/plenary.nvim",
			"antoinemadec/FixCursorHold.nvim",
			"nvim-treesitter/nvim-treesitter",
			{
				"fredrikaverpil/neotest-golang",
				version = "*",
				build = function()
					vim.system({ "go", "install", "gotest.tools/gotestsum@latest" }):wait()
				end,
				dependencies = {
					"andythigpen/nvim-coverage", -- Added dependency
				},
			},
			"nvim-neotest/neotest-jest",
			"nvim-neotest/neotest-python",
			"rouge8/neotest-rust",
		},
		opts = function()
			return {
				-- your neotest config here
				adapters = {
					require("neotest-golang")({
						runner = "gotestsum",
						warn_test_name_dupes = false,
						go_test_args = {
							"-v",
							"-race",
							"-count=1",
							"-coverprofile=" .. vim.fn.getcwd() .. "/coverage.out",
						},
					}),
					require("neotest-jest"),
					require("neotest-python"),
					require("neotest-rust"),
				},
			}
		end,
		config = function(_, opts)
			require("neotest").setup(opts)
		end,
	},
	{
		"andythigpen/nvim-coverage",
		version = "*",
		config = function()
			require("coverage").setup({
				auto_reload = true,
			})
		end,
	},
}
