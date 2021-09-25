" Key binding to run command
let g:ctrlp_map = '<leader>f'

" Used to specifiy root directory of all searches
" by using the cwd of any file in the list
let g:ctrlp_root_markers = ['pom.xml', 'package.yaml', 'gradlew', 'compile_commands.json', '.config']

" Open file already opened, in new buffer instead of switching to it
let g:ctrlp_switch_buffer = 'et'

" Exclude files in .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
