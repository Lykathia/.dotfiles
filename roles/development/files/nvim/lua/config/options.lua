vim.g.mapleader = ","

-- Remove animations
vim.g.snacks_animate = false

local opt = vim.opt

opt.list = false

opt.expandtab = true
opt.smarttab = true
opt.shiftwidth = 4
opt.tabstop = 4
opt.lbr = true

opt.number = true
opt.relativenumber = false

opt.ignorecase = true
opt.hlsearch = true
opt.incsearch = true

vim.keymap.set("n", "<leader>r", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<leader>a", vim.lsp.buf.code_action, opts)
