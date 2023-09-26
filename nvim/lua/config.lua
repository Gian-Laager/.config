require('rose-pine').setup({
    -- disable_background = true
})

require('zen-mode').setup({
    window = {
        width = 150
    }
})

vim.cmd.colorscheme("onedark")

require 'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { "cpp", "c", "python", "rust", "java", "haskell" },
    auto_install = true,
    highlight = {
        enable = true
    }
}
require('trouble').setup {
    height = 10,                    -- height of the trouble list
    icons = true,                   -- use devicons for filenames
    mode = "workspace_diagnostics", -- "lsp_workspace_diagnostics", "lsp_document_diagnostics", "quickfix", "lsp_references", "loclist"
    fold_open = "",              -- icon used for open folds
    fold_closed = "",            -- icon used for closed folds
    action_keys = {
        -- key mappings for actions in the trouble list
        close = "q",                  -- close the list
        cancel = "<esc>",             -- cancel the preview and get back to your last window / buffer / cursor
        refresh = "r",                -- manually refresh
        jump = { "<cr>", "<tab>" },   -- jump to the diagnostic or open / close folds
        jump_close = { "o" },         -- jump to the diagnostic and close the list
        toggle_mode = "m",            -- toggle between "workspace" and "document" diagnostics mode
        toggle_preview = "P",         -- toggle auto_preview
        hover = "K",                  -- opens a small poup with the full multiline message
        preview = "p",                -- preview the diagnostic location
        close_folds = { "zM", "zm" }, -- close all folds
        open_folds = { "zR", "zr" },  -- open all folds
        toggle_fold = { "zA", "za" }, -- toggle fold of current file
        previous = "k",               -- preview item
        next = "j"                    -- next item
    },
    indent_lines = true,              -- add an indent guide below the fold icons
    auto_open = false,                -- automatically open the list when you have diagnostics
    auto_close = false,               -- automatically close the list when you have no diagnostics
    auto_preview = true,              -- automatyically preview the location of the diagnostic. <esc> to close preview and go back to last window
    auto_fold = false,                -- automatically fold a file trouble list at creation
    signs = {
        -- icons / text used for a diagnostic
        error = "",
        warning = "",
        hint = "",
        information = "",
        other = "﫠"
    },
    use_lsp_diagnostic_signs = false -- enabling this will use the signs defined in your lsp client
}

require 'nvim-web-devicons'.setup {}
require('lualine').setup()
require('nvim_comment').setup()
require("telescope").load_extension('harpoon')
require("harpoon").setup {
    menu = {
        width = math.floor(vim.api.nvim_win_get_width(0) / 2),
    }
}

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<leader>pd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<space><tab>', require("harpoon.ui").toggle_quick_menu, opts)
vim.keymap.set('n', '<space>m', require("harpoon.mark").add_file, opts)
vim.keymap.set('n', '<space>1', function() require("harpoon.ui").nav_file(1) end, opts)
vim.keymap.set('n', '<space>2', function() require("harpoon.ui").nav_file(2) end, opts)
vim.keymap.set('n', '<space>3', function() require("harpoon.ui").nav_file(3) end, opts)
vim.keymap.set('n', '<space>4', function() require("harpoon.ui").nav_file(4) end, opts)
vim.keymap.set('n', '<space>5', function() require("harpoon.ui").nav_file(5) end, opts)
vim.keymap.set('n', '<space>6', function() require("harpoon.ui").nav_file(6) end, opts)
vim.keymap.set('n', '<space>7', function() require("harpoon.ui").nav_file(7) end, opts)
vim.keymap.set('n', '<space>8', function() require("harpoon.ui").nav_file(8) end, opts)
vim.keymap.set('n', '<space>9', function() require("harpoon.ui").nav_file(9) end, opts)
vim.keymap.set('n', '<space>0', function() require("harpoon.ui").nav_file(0) end, opts)
vim.keymap.set('n', '<space>e', require("harpoon.ui").nav_prev, opts)
vim.keymap.set('n', '<space>w', require("harpoon.ui").nav_next, opts)
vim.keymap.set('n', '<space>M', require("harpoon.mark").rm_file, opts)
vim.keymap.set('n', '<space>C', require("harpoon.mark").clear_all, opts)
vim.keymap.set('n', '<space>u', require("harpoon.mark").toggle_file, opts)

vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

vim.keymap.set('n', '<space>db', function() vim.cmd("DapToggleBreakpoint") end, opts)
vim.keymap.set('n', '<space>ds', function() vim.cmd("DapStepOver") end, opts)
vim.keymap.set('n', '<space>di', function() vim.cmd("DapStepInto") end, opts)
vim.keymap.set('n', '<space>do', function() vim.cmd("DapStepOut") end, opts)
vim.keymap.set('n', '<space>dt', function() vim.cmd("DapTerminate") end, opts)
vim.keymap.set('n', '<space>dc', function() vim.cmd("DapContinue") end, opts)
vim.keymap.set('n', '<space>dq', function()
    vim.cmd("DapTerminate") 
    require'dapui'.close()
end, opts)

vim.keymap.set('n', '<space>gm', '<cmd>Magit<CR>', opts)

local lsp = require('lsp-zero')

lsp.preset('recommended')

lsp.set_preferences({
    sign_icons = {
        error = "",
        warn = "",
        hint = "",
        info = "",
    },
})

local builtin = require("telescope.builtin")

lsp.on_attach(function(client, bufnr)
    local opts = { buffer = bufnr, remap = false }

    vim.keymap.set("n", 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set("n", 'gd', builtin.lsp_definitions, opts)

    vim.keymap.set("n", 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", 'gi', builtin.lsp_implementations, opts)

    vim.keymap.set("n", 'go', builtin.lsp_type_definitions, opts)
    vim.keymap.set("n", 'gr', builtin.lsp_references, opts)
    vim.keymap.set("n", '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set("n", '<leader>ca', "<cmd>CodeActionMenu<cr>", opts)
    vim.keymap.set("v", '<leader>ca', "<cmd>CodeActionMenu<cr>", opts)
    vim.keymap.set("n", '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set("i", '<C-k>', vim.lsp.buf.signature_help, opts)

    vim.keymap.set("n", 'gl', vim.diagnostic.open_float, opts)
    vim.keymap.set("n", '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", '<leader>cf', "<cmd>LspZeroFormat<cr>", opts)
end)

lsp.setup_nvim_cmp({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'vsnip' },
        { name = 'path' },
        { name = 'buffer' },
    }
})

lsp.setup()

require 'lspconfig'.eslint.setup {
    filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx",
        "vue", "svelte", "astro" },
    root_dir = function() return vim.loop.cwd() end -- run lsp for javascript in any directory
}

vim.fn.sign_define('DapBreakpoint', {text='●', texthl='red', linehl='', numhl=''})

require("dapui").setup()

require('nvim-dap-projects').search_project_config()

require("mason-nvim-dap").setup()

local dap, dapui = require("dap"), require("dapui")
dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end

-- dap
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end

local cmp = require 'cmp'
cmp.setup({
    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        -- Add tab support
        ['<S-Tab>'] = cmp.mapping.select_prev_item(),
        ['<Tab>'] = cmp.mapping.select_next_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        })
    },
})
vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics,
        { update_in_insert = true })

require("telescope").load_extension("refactoring")

require('gitsigns').setup {
  signs = {
    add          = { text = '+' },
    change       = { text = '│' },
    delete       = { text = '_' },
    topdelete    = { text = '‾' },
    changedelete = { text = '~' },
    untracked    = { text = '┆' },
  },
}

-- remap to open the Telescope refactoring menu in visual mode
vim.api.nvim_set_keymap(
	"v",
	"<leader>rr",
	"<Esc><cmd>lua require('telescope').extensions.refactoring.refactors()<CR>",
	{ noremap = true }
)

require("neotest-gtest").setup({})
require("neotest").setup({
  adapters = {
    require("neotest-gtest")
  }
})
