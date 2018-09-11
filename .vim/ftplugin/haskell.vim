call Tabs(4)

set wildignore+=*.hi,*.o,*.js_dyn_hi,*.js_dyn_o,*.js_hi,*.js_o,*.dyn_hi,*.dyn_o,*.cache

let g:ale_linters = { 'haskell': ['hie'] }
let g:ale_fixers = { 'haskell': ['brittany'] }
let g:ale_completion_enabled = 1
