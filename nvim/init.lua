-- Set leader key
vim.g.mapleader = " "

-- Insert mode mappings
vim.keymap.set('i', 'jj', '<Esc>')

-- Normal mode mappings
vim.keymap.set('n', '<leader>h', '<C-w>h')
vim.keymap.set('n', '<leader>j', '<C-w>j')
vim.keymap.set('n', '<leader>k', '<C-w>k')
vim.keymap.set('n', '<leader>l', '<C-w>l')

-- Terminal mode mappings
vim.keymap.set('t', '<A-h>', '<C-\\><C-N><C-w>h')
vim.keymap.set('t', '<A-j>', '<C-\\><C-N><C-w>j')
vim.keymap.set('t', '<A-k>', '<C-\\><C-N><C-w>k')
vim.keymap.set('t', '<A-l>', '<C-\\><C-N><C-w>l')

vim.keymap.set('t', '<A-a>', '<C-\\><C-N>:bp<CR>')
vim.keymap.set('t', '<A-f>', '<C-\\><C-N>:bn<CR>')

vim.keymap.set('t', '<Esc>', '<C-\\><C-n>')
vim.keymap.set('t', 'jj', '<C-\\><C-n>')

-- Window resizing
vim.keymap.set('n', '<leader><', ':vertical resize -5<CR>')
vim.keymap.set('n', '<leader>>', ':vertical resize +5<CR>')

-- Buffer navigation
vim.keymap.set('n', '<leader>a', ':bp<CR>')
vim.keymap.set('n', '<leader>f', ':bn<CR>')

-- Scrolling and searching
vim.keymap.set('n', '<C-d>', '<C-d>zz')
vim.keymap.set('n', '<C-u>', '<C-u>zz')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')

-- Set line numbers
vim.opt.number = true
vim.opt.relativenumber = true

-- Undo and swap settings
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim"
vim.opt.undofile = true
vim.opt.swapfile = false

-- Indentation settings
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

-- Search and display settings
vim.opt.smartcase = true
vim.opt.wrap = false
vim.opt.incsearch = true
vim.opt.hlsearch = false
vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "120"
vim.opt.termguicolors = true

-- Define diagnostic signs
vim.fn.sign_define("DiagnosticSignError", { text = "", texthl = "DiagnosticSignError", linehl = "", numhl = "" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "", texthl = "DiagnosticSignWarn", linehl = "", numhl = "" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "", texthl = "DiagnosticSignInfo", linehl = "", numhl = "" })
vim.fn.sign_define("DiagnosticSignHint", { text = "", texthl = "DiagnosticSignHint", linehl = "", numhl = "" })

function file_exists(name)
    local file = io.open(name, "r")        -- Try to open the file in read mode
    if file then
        local ok, err, code = file:read(1) -- Try reading a byte
        file:close()                       -- Close the file regardless
        if code == 21 then                 -- Error code for a directory
            return false
        end
        return true
    end
    return false
end

if file_exists(".nvim.lua") then
    vim.cmd("luafile .nvim.lua")
elseif file_exists(".vim") then
    vim.cmd("source .vim")
end

local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.8',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }

    use 'nvim-tree/nvim-web-devicons'
    use 'ryanoasis/vim-devicons'

    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-buffer'
    use 'ThePrimeagen/harpoon'
    use 'kuangliu/onedark.vim'
    use 'EdenEast/nightfox.nvim'
    use 'kassio/neoterm'
    use 'jreybert/vimagit'
    use 'terrortylor/nvim-comment'
    use 'preservim/nerdtree'

    use 'williamboman/mason.nvim'
    use 'mbbill/undotree'
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
    }

    use 'lewis6991/gitsigns.nvim'
    use 'neovim/nvim-lspconfig'
    use 'hrsh7th/cmp-nvim-lsp'

    use {
        'rcarriga/nvim-dap-ui',
        requires = { { 'nvim-neotest/nvim-nio', 'mfussenegger/nvim-dap' } },
    }
    use 'nvim-lualine/lualine.nvim'
    use 'ThePrimeagen/vim-be-good'
    use 'lervag/vimtex'
    use 'github/copilot.vim'
    use 'petRUShka/vim-opencl'
    use 'nvimdev/lspsaga.nvim'

    if packer_bootstrap then
        require('packer').sync()
    end
end)

require('nvim-web-devicons').setup()
vim.g.NERDTreeShowIcons = 1

vim.cmd [[colorscheme onedark]]

require 'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { "cpp", "c", "python" },
    ignore_install = { "latex" },
    auto_install = true,
    highlight = {
        enable = true,
    }
}

require("harpoon").setup {
    menu = {
        width = math.floor(vim.api.nvim_win_get_width(0) / 2),
    }
}

require('telescope').setup {}
require('mason').setup()

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

require('lualine').setup {}

require('nvim_comment').setup({
    hook = function()
        if vim.api.nvim_buf_get_option(0, "filetype") == "glsl" then
            vim.api.nvim_buf_set_option(0, "commentstring", "// %s")
        elseif vim.bo.filetype == 'cl' then
            vim.bo.commentstring = '// %s'
        elseif vim.bo.filetype == 'cpp' then
            vim.bo.commentstring = '// %s'
        elseif vim.bo.filetype == 'c' then
            vim.bo.commentstring = '// %s'
        end
    end
})

-- Dap
require("dapui").setup {}

vim.fn.sign_define('DapBreakpoint', { text = '●', texthl = 'red', linehl = '', numhl = '' })

local dap, dapui = require("dap"), require("dapui")
dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end

-- key bindings
local opts = { noremap = true, silent = true }

local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader>pp', builtin.git_files, { desc = 'Telescope find git files' }, opts)
vim.keymap.set('n', '<leader>pe', builtin.find_files, { desc = 'Telescope find files' }, opts)
vim.keymap.set('n', '<leader>pg', builtin.live_grep, { desc = 'Telescope live grep' }, opts)
vim.keymap.set('n', '<leader>pb', builtin.buffers, { desc = 'Telescope find symbol' }, opts)
vim.keymap.set('n', '<leader>po', builtin.treesitter, { desc = 'Telescope find symbol' }, opts)
vim.keymap.set('n', '<leader>pd', builtin.diagnostics, opts)

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

vim.keymap.set('n', '<space>gm', '<cmd>Magit<CR>', opts)

vim.keymap.set('n', '<space>db', function() vim.cmd("DapToggleBreakpoint") end, opts)
vim.keymap.set('n', '<space>ds', function() vim.cmd("DapStepOver") end, opts)
vim.keymap.set('n', '<space>di', function() vim.cmd("DapStepInto") end, opts)
vim.keymap.set('n', '<space>do', function() vim.cmd("DapStepOut") end, opts)
vim.keymap.set('n', '<space>dt', function() vim.cmd("DapTerminate") end, opts)
vim.keymap.set('n', '<space>dc', function() vim.cmd("DapContinue") end, opts)
vim.keymap.set('n', '<space>dq', function()
    vim.cmd("DapTerminate")
    require 'dapui'.close()
end, opts)


vim.api.nvim_set_keymap('i', '<C-J>', 'copilot#Accept("\\<CR>")', { expr = true, silent = true, script = true })

local cmp = require 'cmp'
cmp.setup {
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },

    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'path' },
    },

    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
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
}


vim.api.nvim_set_keymap('n', '<leader>t', ':Topen<CR>', { noremap = true, silent = true })

-- Define the on_attach function
local on_attach = function(client, bufnr)
    local bufmap = function(mode, lhs, rhs)
        local opts = { buffer = bufnr, silent = true }
        vim.keymap.set(mode, lhs, rhs, opts)
    end

    -- Key mappings for LSP functions
    bufmap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')     -- Go to definition
    bufmap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>') -- Go to implementation
    bufmap('n', '<C-k>', vim.lsp.buf.hover)
    bufmap('n', 'K', vim.lsp.buf.hover)
    bufmap('i', '<C-k>', vim.lsp.buf.signature_help)
    bufmap('n', '<leader>rn', '<cmd>:Lspsaga rename<CR>')      -- Rename symbol
    -- bufmap('n', '<leader>rn', nvim.lsp.buf.rename)    -- Rename symbol
    bufmap('n', '<leader>ca', '<cmd>:Lspsaga code_action<CR>') -- Code actions
    -- bufmap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>') -- Code actions
    bufmap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>') -- Show references
    bufmap('n', '<leader>cf', '<cmd>lua vim.lsp.buf.format()<CR>')
end

-- Set up LSP servers with lspconfig
vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics,
        { update_in_insert = true })


local lspconfig = require('lspconfig')
local capabilities = require('cmp_nvim_lsp').default_capabilities()

lspconfig.fortls.setup {
    root_dir = function(fname)
        return lspconfig.util.root_pattern('compile_commands.json')(fname) or
            lspconfig.util.find_git_ancestor(fname)
    end,
    settings = {
        fortls = {
            commandPath = '',
        }
    }
}

require('lspconfig').pyright.setup {}

require 'lspconfig'.jdtls.setup {
    init_options = {
        extendedClientCapabilities = {
            classFileContentsSupport = true
        }
    }
}

require 'lspconfig'.clangd.setup {
    cmd = { "clangd", '--background-index', '--clang-tidy', '--clang-tidy-checks=\\*' },
}

require('lspconfig').lua_ls.setup {}

require 'lspconfig'.glsl_analyzer.setup {
    filetypes = { "glsl", "vert", "tesc", "tese", "frag", "geom", "comp" }
}

require 'lspconfig'.opencl_ls.setup {
    filetypes = { "opencl" }
}

require 'lspconfig'.hls.setup {
    cmd = { "stack", "exec", "--", "haskell-language-server-wrapper", "--lsp" },
    filetypes = { 'haskell', 'lhaskell', 'cabal' },
}
vim.g.hls_formatting = 1
vim.g.hls_indent_size = 2
vim.api.nvim_create_autocmd("FileType", {
    pattern = "haskell",
    callback = function()
        vim.bo.tabstop = 2
        vim.bo.softtabstop = 2
        vim.bo.shiftwidth = 2
        vim.bo.expandtab = true
        vim.bo.autoindent = true
    end,
})

local servers = {
    'jdtls',
    'clangd',
    'rust_analyzer',
    'pyright',
    'lua_ls',
    'cmake',
    'fortls',
    'jsonls',
    'ltex',
    'taplo',
    'wgsl_analyzer',
    'bashls',
    'hls',
}

for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
        capabilities = capabilities,
        on_attach = on_attach,
        flags = {
            debounce_text_changes = 150,
        }
    }
end


require('lspsaga').setup({
    code_action = {
        num_shortcut = true,
        show_server_name = false,
        extend_gitsigns = false,
        keys = {
            quit = 'q',
            exec = '<CR>',
        },
    },
    lightbulb = {
        enable = false,
    },

    symbol_in_winbar = {
        enable = false,
    },
    beacon = {
        enable = false,
    },
})

-- Shader Stuff
vim.cmd [[
autocmd BufRead,BufNewFile *.cl set filetype=opencl
]]
vim.cmd [[
autocmd BufRead,BufNewFile *.vert set filetype=glsl
]]

vim.cmd [[
autocmd BufRead,BufNewFile *.frag set filetype=glsl
]]

vim.cmd [[
autocmd BufRead,BufNewFile *.comp set filetype=glsl
]]

vim.cmd [[
autocmd BufNewFile,BufRead *.wgsl set filetype=wgsl
]]

-- LaTeX and spellchecking
vim.opt_local.spell = false
vim.opt.spelloptions = "camel"
vim.opt.spellcapcheck = "" -- don't check for capital letters at start of sentence

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "tex", "latex" },
    callback = function()
        vim.opt_local.spell = true
        vim.opt.spelloptions = "camel"
        vim.opt.spellcapcheck = "" -- don't check for capital letters at start of sentence
    end
})

vim.g.vimtex_view_method = 'zathura'
vim.g.vimtex_view_general_viewer = 'okular'
vim.g.vimtex_view_general_options = '--unique file:@pdf\\#src:@line@tex'
vim.g.vimtex_compiler_method = 'latexmk'

-- Copliot
vim.g.copilot_no_tab_map = true
vim.g.copilot_enabled = false
