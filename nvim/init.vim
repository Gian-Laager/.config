let mapleader=" "

imap jj <esc>

nmap <leader>h <C-w>h
nmap <leader>j <C-w>j
nmap <leader>k <C-w>k
nmap <leader>l <C-w>l

tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l

tnoremap <A-a> <C-\><C-N>:bp<CR>
tnoremap <A-f> <C-\><C-N>:bn<CR>

tnoremap <Esc> <C-\><C-n>
tnoremap jj <C-\><C-n>


nmap <leader>< :vertical resize -5<CR>
nmap <leader>> :vertical resize +5<CR>

nmap <leader>a :bp<CR>
nmap <leader>f :bn<CR>

nmap <C-d> <C-d>zz
nmap <C-u> <C-u>zz
nmap n nzzzv
nmap N Nzzzv


set number relativenumber

" :augroup numbertoggle
" :  autocmd!
" :  autocmd BufEnter,WinEnter * if &nu && mode() != "i" | set rnu   | endif
" :  autocmd BufLeave,WinLeave   * if &nu                | set nornu | endif
" :augroup END

set undodir=~/.cache/vim
set undofile
set noswapfile

set tabstop=4               
set softtabstop=4           
set shiftwidth=4            

set expandtab               
set autoindent              

set smartcase
set nowrap
set incsearch
set nohlsearch
set scrolloff=8
set signcolumn=yes
set colorcolumn=120

call plug#begin("~/.vim/plugged")
Plug 'nvim-telescope/telescope.nvim'
" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/nvim-cmp'

" Completion framework
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'morhetz/gruvbox'

" LSP completion source for nvim-cmp
Plug 'hrsh7th/cmp-vsnip'
Plug 'terrortylor/nvim-comment'
Plug 'preservim/nerdtree'
Plug 'nvim-lua/plenary.nvim' " don't forget to add this one if you don't have it yet!
Plug 'ThePrimeagen/harpoon'
Plug 'doums/darcula'
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'
Plug 'nvim-lualine/lualine.nvim'
" If you want to have icons in your statusline choose one of these
Plug 'kyazdani42/nvim-web-devicons'
Plug 'ryanoasis/vim-devicons'
Plug 'folke/lsp-trouble.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'ThePrimeagen/vim-be-good'

Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'

Plug 'saadparwaiz1/cmp_luasnip'
Plug 'hrsh7th/cmp-nvim-lua'

Plug 'L3MON4D3/LuaSnip'
Plug 'rafamadriz/friendly-snippets'

Plug 'VonHeikemen/lsp-zero.nvim'

Plug 'EdenEast/nightfox.nvim'
Plug 'rose-pine/neovim'
Plug 'ivanlhz/vim-electron'
Plug 'casonadams/walh'

Plug 'mbbill/undotree'
Plug 'folke/zen-mode.nvim'
Plug 'weilbith/nvim-code-action-menu'

Plug 'mfussenegger/nvim-dap'
Plug 'mfussenegger/nvim-dap'
Plug 'rcarriga/nvim-dap-ui'
Plug 'jayp0521/mason-nvim-dap.nvim'
Plug 'ldelossa/nvim-dap-projects'
call plug#end()

" set spell spelllang=en_us " spell checking
set termguicolors

let g:NERDTreeDirArrowExpandable = ''
let g:NERDTreeDirArrowCollapsible = ''

let NERDTreeStatusline="%{exists('b:NERDTree')?fnamemodify(b:NERDTree.root.path.str(), ':~'):''}"

nnoremap <leader>pp <cmd>Telescope git_files<cr>
nnoremap <leader>pe <cmd>Telescope find_files<cr>
nnoremap <leader>pg <cmd>Telescope live_grep<cr>
nnoremap <leader>pb <cmd>Telescope buffers<cr>
nnoremap <Leader>po <cmd>lua require'telescope.builtin'.treesitter{}<cr>
nnoremap <leader>ph <cmd>Telescope help_tags<cr>
nnoremap <leader>pm <cmd>Telescope harpoon marks<cr>

nnoremap <leader>pu <cmd>UndotreeToggle<cr>
nnoremap <leader>gz <cmd>ZenMode<cr>


sign define DiagnosticSignError text= linehl= texthl=DiagnosticSignError numhl= 
sign define DiagnosticSignWarn text= linehl= texthl=DiagnosticSignWarn numhl= 
sign define DiagnosticSignInfo text= linehl= texthl=DiagnosticSignInfo numhl= 
sign define DiagnosticSignHint text=  linehl= texthl=DiagnosticSignHint numhl= 

au BufNewFile,BufRead *.wgsl set filetype=wgsl

lua <<EOF
require("config")
EOF

nnoremap <leader>q :TroubleToggle<CR>

call system("test -f .vim")
if  v:shell_error == 0
    so .vim
endif
