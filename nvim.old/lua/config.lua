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
    ensure_installed = { "cpp", "c", "python", "rust", "java", "haskell", "markdown", "markdown_inline" },
    ignore_install = { "latex" },
    auto_install = true,
    highlight = {
        enable = true,
    }
}
require('trouble').setup {
    height = 10, -- height of the trouble list
    icons = true, -- use devicons for filenames
    mode = "workspace_diagnostics", -- "lsp_workspace_diagnostics", "lsp_document_diagnostics", "quickfix", "lsp_references", "loclist"
    fold_open = "", -- icon used for open folds
    fold_closed = "", -- icon used for closed folds
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
    require 'dapui'.close()
end, opts)

vim.keymap.set('n', '<space>gm', '<cmd>Magit<CR>', opts)

-- require("fzf-lua").setup({ fzf_colors = true })

local lsp = require('lsp-zero')

-- lsp.preset('recommended')
--
-- lsp.set_preferences({
--     sign_icons = {
--         error = "",
--         warn = "",
--         hint = "",
--         info = "",
--     },
-- })


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

local builtin = require("telescope.builtin")

lsp.on_attach(function(client, bufnr)
    local opts = { buffer = bufnr, remap = false }

    vim.keymap.set("n", 'K', "<cmd>Lspsaga hover_doc<CR>", opts)
    vim.keymap.set("n", 'gd', vim.lsp.buf.definition, opts)

    vim.keymap.set("n", 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set("n", 'gi', builtin.lsp_implementations, opts)

    vim.keymap.set("n", 'go', builtin.lsp_type_definitions, opts)
    vim.keymap.set("n", 'gr', builtin.lsp_references, opts)
    vim.keymap.set("n", '<leader>rn', "<cmd>Lspsaga rename<CR>", opts)
    -- vim.keymap.set("n", '<leader>ca', "<cmd>CodeActionMenu<cr>", opts)
    -- vim.keymap.set("n", '<leader>ca', vim.lsp.buf.code_action, opts)
    -- vim.keymap.set("n", '<leader>ca', require("actions-preview").code_actions, opts)
    -- vim.keymap.set("n", '<leader>ca', require("fzf-lua").lsp_code_actions, opts)
    vim.keymap.set("n", '<leader>ca', "<cmd>Lspsaga code_action<CR>", opts)
    
    -- vim.keymap.set("n", '<C-k>', vim.lsp.buf.signature_help, opts)
    -- vim.keymap.set("i", '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set("n", '<C-k>', "<cmd>Lspsaga hover_doc<CR>", opts)
    vim.keymap.set("i", '<C-k>', "<cmd>Lspsaga hover_doc<CR>", opts)

    vim.keymap.set("n", 'gl', vim.diagnostic.open_float, opts)
    vim.keymap.set("n", '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set("n", ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set("n", '<leader>cf', "<cmd>LspZeroFormat<cr>", opts)
end)

lsp.setup()

require 'lspconfig'.eslint.setup {
    filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx",
        "vue", "svelte", "astro" },
    root_dir = function() return vim.loop.cwd() end -- run lsp for javascript in any directory
}


require 'lspconfig'.jdtls.setup {
    init_options = {
        extendedClientCapabilities = {
            classFileContentsSupport = true
        }
    }
}

require 'lspconfig'.hls.setup {
    cmd = { "stack", "exec", "--", "haskell-language-server-wrapper", "--lsp" },
    filetypes = { 'haskell', 'lhaskell', 'cabal' },
}

require'lspconfig'.clangd.setup{
    cmd = { "clangd", '--background-index', '--clang-tidy', '--clang-tidy-checks=\\*' },
}

local function get_compile_commands()
  local compile_commands = vim.fn.findfile("compile_commands.json", ".;")
  if compile_commands ~= "" then
    return compile_commands
  end
  return nil
end

local lspconfig = require('lspconfig')

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

vim.cmd [[
  autocmd BufRead,BufNewFile *.cl set filetype=opencl
]]

require'lspconfig'.opencl_ls.setup{
    filetypes = {"opencl"}
}

vim.cmd [[
  autocmd BufRead,BufNewFile *.vert set filetype=glsl
]]

vim.cmd [[
  autocmd BufRead,BufNewFile *.frag set filetype=glsl
]]

vim.cmd [[
  autocmd BufRead,BufNewFile *.comp set filetype=glsl
]]


require'lspconfig'.glsl_analyzer.setup{
  filetypes = { "glsl", "vert", "tesc", "tese", "frag", "geom", "comp" }
}

require('lspconfig').pyright.setup{}

vim.fn.sign_define('DapBreakpoint', { text = '●', texthl = 'red', linehl = '', numhl = '' })

require("dapui").setup()

require('nvim-dap-projects').search_project_config()

require("mason-nvim-dap").setup()
require("mason").setup()

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
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) 
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    -- sources = {
    --     { name = 'nvim_lsp' },
    --     { name = 'vsnip' },
    --     { name = 'path' },
    --     { name = 'buffer' },
    -- },

    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' },
    }, {
      { name = 'buffer' },
      { name = 'path' },
    }),

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

vim.keymap.set("n", "gf", function()
    if require("obsidian").util.cursor_on_markdown_link() then
        return "<cmd>ObsidianFollowLink<CR>"
    else
        return "gf"
    end
end, { noremap = false, expr = true })


local prompts = require('CopilotChat.prompts')
local select = require('CopilotChat.select')

require("CopilotChat").setup {
  debug = false, -- Enable debug logging
  proxy = nil, -- [protocol://]host[:port] Use this proxy
  allow_insecure = false, -- Allow insecure server connections

  system_prompt = prompts.COPILOT_INSTRUCTIONS, -- System prompt to use
  model = 'gpt-4', -- GPT model to use, 'gpt-3.5-turbo' or 'gpt-4'
  temperature = 0.1, -- GPT temperature

  question_header = '## User ', -- Header to use for user questions
  answer_header = '## Copilot ', -- Header to use for AI answers
  error_header = '## Error ', -- Header to use for errors
  separator = '---', -- Separator to use in chat

  show_folds = true, -- Shows folds for sections in chat
  show_help = false, -- Shows help message as virtual lines when waiting for user input
  auto_follow_cursor = true, -- Auto-follow cursor in chat
  auto_insert_mode = false, -- Automatically enter insert mode when opening window and if auto follow cursor is enabled on new prompt
  clear_chat_on_new_prompt = false, -- Clears chat on every new prompt

  context = nil, -- Default context to use, 'buffers', 'buffer' or none (can be specified manually in prompt via @).
  history_path = vim.fn.stdpath('data') .. '/copilotchat_history', -- Default path to stored history
  callback = nil, -- Callback to use when ask response is received

  -- default selection (visual or line)
  selection = function(source)
    return select.visual(source) or select.line(source)
  end,

  -- default prompts
  prompts = {
    Explain = {
      prompt = '/COPILOT_EXPLAIN Write an explanation for the active selection as paragraphs of text.',
    },
    Review = {
      prompt = '/COPILOT_REVIEW Review the selected code.',
      callback = function(response, source)
        -- see config.lua for implementation
      end,
    },
    Fix = {
      prompt = '/COPILOT_GENERATE There is a problem in this code. Rewrite the code to show it with the bug fixed.',
    },
    Optimize = {
      prompt = '/COPILOT_GENERATE Optimize the selected code to improve performance and readablilty.',
    },
    Docs = {
      prompt = '/COPILOT_GENERATE Please add documentation comment for the selection.',
    },
    Tests = {
      prompt = '/COPILOT_GENERATE Please generate tests for my code.',
    },
    FixDiagnostic = {
      prompt = 'Please assist with the following diagnostic issue in file:',
      selection = select.diagnostics,
    },
    Commit = {
      prompt = 'Write commit message for the change with commitizen convention. Make sure the title has maximum 50 characters and message is wrapped at 72 characters. Wrap the whole message in code block with language gitcommit.',
      selection = select.gitdiff,
    },
    CommitStaged = {
      prompt = 'Write commit message for the change with commitizen convention. Make sure the title has maximum 50 characters and message is wrapped at 72 characters. Wrap the whole message in code block with language gitcommit.',
      selection = function(source)
        return select.gitdiff(source, true)
      end,
    },
  },

  -- default window options
    window = {
        layout = 'float', -- 'vertical', 'horizontal', 'float', 'replace'
        width = 0.7,        -- fractional width of parent, or absolute width in columns when > 1
        height = 0.7,       -- fractional height of parent, or absolute height in rows when > 1
        -- Options below only apply to floating windows
        relative = 'editor', -- 'editor', 'win', 'cursor', 'mouse'
        border = 'single',  -- 'none', single', 'double', 'rounded', 'solid', 'shadow'
        row = nil,          -- row position of the window, default is centered
        col = nil,          -- column position of the window, default is centered
        title = 'Copilot Chat', -- title of chat window
        footer = nil,       -- footer of chat window
        zindex = 1,         -- determines if window is on top or below other floating windows
    },

  -- default mappings
  mappings = {
    complete = {
      detail = 'Use @<Tab> or /<Tab> for options.',
      insert ='<Tab>',
    },
    close = {
      normal = 'q',
      insert = '<C-c>'
    },
    reset = {
      normal ='<C-l>',
      insert = '<C-l>'
    },
    submit_prompt = {
      normal = '<CR>',
      insert = '<Esc>[13;2u>'-- '<S-CR>'
    },
    accept_diff = {
      normal = '<C-j>',
      insert = '<C-j>'
    },
    yank_diff = {
      normal = 'gy',
    },
    show_diff = {
      normal = 'gd'
    },
    show_system_prompt = {
      normal = 'gp'
    },
    show_user_selection = {
      normal = 'gs'
    },
  },
}

vim.api.nvim_set_keymap('n', '<leader>ao', ':CopilotChatToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>ao', ':CopilotChatToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>af', ':CopilotChatFix<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>ar', ':CopilotChatReview<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>at', ':CopilotChatTests<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<leader>ae', ':CopilotChatExplain<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ax', ':CopilotChatStop<CR>', { noremap = true, silent = true })


vim.opt_local.spell = true
vim.opt.spelloptions = "camel"
vim.opt.spellcapcheck = ""  -- don't check for capital letters at start of sentence

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"tex", "latex"},
  callback = function()
    vim.opt_local.spell = true
    vim.opt.spelloptions = "camel"
    vim.opt.spellcapcheck = ""  -- don't check for capital letters at start of sentence
  end
})

vim.api.nvim_set_keymap('n', '<leader>t', ':Topen<CR>', { noremap = true, silent = true })
