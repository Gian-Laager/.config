vim.g.mapleader = " "

vim.keymap.set('i', 'jj', '<Esc>')

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.autoindent = true

vim.opt.signcolumn = 'yes'
vim.opt.wrap = false
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = false

vim.opt.number = true
vim.opt.relativenumber = true

vim.keymap.set('t', '<Esc>', '<C-\\><C-n>')
vim.keymap.set('t', 'jj', '<C-\\><C-n>')

vim.keymap.set('n', '<leader>l', '<C-w>l')
vim.keymap.set('n', '<leader>k', '<C-w>k')
vim.keymap.set('n', '<leader>j', '<C-w>j')
vim.keymap.set('n', '<leader>h', '<C-w>h')

local terminal_buf = nil 

function open_terminal() 
    if terminal_buf and vim.api.nvim_buf_is_valid(terminal_buf) then 
        vim.api.nvim_set_current_buf(terminal_buf)
    else 
        vim.cmd('terminal')
        terminal_buf = vim.api.nvim_get_current_buf()
    end
end 

function send_to_terminal(cmd)
    if terminal_buf and vim.api.nvim_buf_is_valid(terminal_buf) then 
        vim.fn.chansend(vim.b[terminal_buf].terminal_job_id, cmd .. "\n")
    else 
        open_terminal() 
        send_to_terminal(cmd)
    end 
end

vim.api.nvim_create_user_command('T', function(opts) 
    send_to_terminal(opts.args)
end, {nargs = '+' })

vim.keymap.set('n', '<leader>t', open_terminal)

vim.keymap.set('n', '<leader>pe', ':edit ')
vim.keymap.set('n', '<leader>pb', ':buffer ')

vim.keymap.set('i', '<C-Space>', '<C-x><C-o>')

local autocmd = vim.api.nvim_create_autocmd

vim.opt.completeopt:remove("preview")
vim.diagnostic.config({ update_in_insert = true })
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)

local on_lsp_attatch = function(opts) 
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<leader>cf', vim.lsp.buf.format, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
end 

if vim.fn.executable('clangd') == 1 then 
    autocmd("FileType", {
        pattern = {'c', 'cpp'},
        callback = function()
            local root_dir = vim.fs.dirname(
                vim.fs.find({'compiile_commands.json', '.git'}, {upward = true })[1]
            ) or vim.fn.getcwd()

            local client = vim.lsp.start({
                name = 'clangd',
                cmd = { 'clangd' },
                root_dir = root_dir,
            })

            vim.lsp.buf_attach_client(0, client)

            local opts = { noremap=true, silent=true, buffer=0 }
            on_lsp_attatch(opts)
        end,
    })
end

if vim.fn.executable('pylsp') == 1 then 
    autocmd("FileType", {
        pattern = { 'python' },
        callback = function()
            local root_dir = vim.fs.dirname(
                vim.fs.find({ '.git' }, {upward = true })[1]
            ) or vim.fn.getcwd()

            local client = vim.lsp.start({
                name = 'pylsp',
                cmd = { 'pylsp' },
                root_dir = root_dir,
            })

            vim.lsp.buf_attach_client(0, client)

            local opts = { noremap=true, silent=true, buffer=0 }
            on_lsp_attatch(opts)
        end,
    })
end

if vim.fn.filereadable(".vim") == 1 then 
    vim.cmd('source .vim')
end
