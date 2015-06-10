# TODO:
# system dependencies (support multiple package managers)
# split off os-specific stuff (like the stuff in ~/bin that's only relevant to linux)
# only enable ternjs in vim if it's built, same for ycm

HOME = ENV['HOME'] || ENV['USERPROFILE']

BUNDLE_DIR = "#{HOME}/.vim/bundle"
VUNDLE_IDENTIFIER = "#{BUNDLE_DIR}/Vundle.vim/.gitignore"
BIN_DIR = "#{HOME}/bin"

VIM_TERN = "#{HOME}/.vim/bundle/tern_for_vim/node_modules/.bin/tern"
VIM_YCM = "#{HOME}/.vim/bundle/YouCompleteMe/third_party/ycmd/ycm_core.so"

task :default => [:required_tooling, :dotfiles, :bin, :vim]

task :required_tooling do
  ['vim', 'git'].each do |tool|
    next if has_program? tool

    puts "ERROR: Install #{tool}"
    exit 1
  end
end

task :dotfiles

task :vim => [:vundle, VIM_YCM, VIM_TERN]

task :vundle => [VUNDLE_IDENTIFIER]


def multiplatform_symlink(source, target)
  source = File.absolute_path source
  target = File.absolute_path target

  if File.exist? target
    rm target
  end

  begin
    symlink source, target
  rescue NotImplementedError
    `cmd /C mklink /H "#{target.gsub '/', '\\'}" "#{source.gsub '/', '\\'}"`
  end
end

def create_dotfiles_tasks
  Dir.foreach '.' do |f|
    next if f == '.' or f == '..' or f == '.git'
    next unless f.start_with? '.'

    if File.directory? f
      FileList["#{f}/**/*"].each do |f|
        next if File.directory? f
        target = "#{HOME}/#{f}"
        d = File.dirname target
        directory d
        task :dotfiles => target
        file target => d do
          multiplatform_symlink f, target
        end
      end
    else
      target = "#{HOME}/#{f}"
      task :dotfiles => target
      file target => f do
        multiplatform_symlink f, target
      end
    end
  end
end

def create_bin_tasks
  Dir.foreach './bin' do |f|
    next if f == '.' or f == '..' or f == '.git'

    target = "#{HOME}/bin/#{f}"
    from = "bin/#{f}"
    task :bin => target
    file target => [BIN_DIR, from] do
      multiplatform_symlink from, target
    end
  end
end

create_dotfiles_tasks
create_bin_tasks

def has_program?(program)
  ENV['PATH'].split(File::PATH_SEPARATOR).any? do |directory|
    File.executable?(File.join(directory, program.to_s)) or File.executable?(File.join(directory, "#{program.to_s}.exe"))
  end
end

file VUNDLE_IDENTIFIER => [BUNDLE_DIR] do
  sh "git", "clone", "https://github.com/gmarik/Vundle.vim.git", "#{BUNDLE_DIR}/Vundle.vim"
  sh "vim +PluginInstall +qall"
end

directory BUNDLE_DIR
directory BIN_DIR

file VIM_TERN do
  Dir.chdir "#{HOME}/.vim/bundle/tern_for_vim" do
    sh "npm install"
  end
end

file VIM_YCM do
  Dir.chdir "#{HOME}/.vim/bundle/YouCompleteMe" do
    #sh "apt-get install cmake python-dev"
    sh "./install.sh"
  end
end

task :vim_install do
  sh "vim +PluginInstall +qall"
end
