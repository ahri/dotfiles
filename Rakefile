# TODO:
# system dependencies (support multiple package managers)
# split off os-specific stuff (like the stuff in ~/bin that's only relevant to linux)

HOME = ENV['HOME'] || ENV['USERPROFILE']

BUNDLE_DIR = "#{HOME}/.vim/bundle"
BIN_DIR = "#{HOME}/bin"

task :default => [:required_tooling, :dotfiles, :bin, :vim]

task :required_tooling do
  ['vim', 'git'].each do |tool|
    next if has_program? tool

    puts "ERROR: Install #{tool}"
    exit 1
  end
end

desc "Sort out the dotfiles"
task :dotfiles

desc "Configure vim with plugins"
task :vim do
  sh "vim -u .vim/installrc.vim +PlugInstall +qall"
end

def multiplatform_symlink(source, target)
  source = File.absolute_path source
  target = File.absolute_path target

  if File.exist? target
    File.delete target
  end

  begin
    ln_sf source, target
  rescue NotImplementedError, SystemCallError
    `cmd /C mklink /H "#{target.gsub '/', '\\'}" "#{source.gsub '/', '\\'}"`
  end
end

def create_dotfiles_tasks
  Dir.foreach '.' do |f|
    next if f == '.' or f == '..' or f == '.git' or f == '.gitignore' or f =~ /^\..*~$/ or f == '.DS_Store'
    next unless f.start_with? '.'

    if File.directory? f
      FileList["#{f}/**/*"].each do |g|
        next if File.directory? g
        target = "#{HOME}/#{g}"
        d = File.dirname target
        directory d
        task :dotfiles => target
        file target => d do
          multiplatform_symlink g, target
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
    task target => [BIN_DIR, from] do
      next if File.symlink?(target) and
        File.absolute_path(File.readlink(target)) == File.absolute_path(from)

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

directory BUNDLE_DIR
directory BIN_DIR

task :vim_install do
  sh "vim +PluginInstall +qall"
end

# Thanks to @mislav for this, found at https://stackoverflow.com/a/5471032
def which(cmd)
  exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
  ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
    exts.each do |ext|
      exe = File.join(path, "#{cmd}#{ext}")
      return exe if File.executable?(exe) && !File.directory?(exe)
    end
  end
  return nil
end

DOCKER = "/usr/bin/docker"
desc "Install docker"
task :docker => DOCKER
file DOCKER do
  sh "sudo apt update && sudo apt install -y apt-transport-https ca-certificates && apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D"
  File.open("/etc/apt/sources.list.d/docker.list", "w") do |fp|
    fp.puts "deb https://apt.dockerproject.org/repo ubuntu-xenial main"
  end
  sh "sudo apt update && sudo apt install -y linux-image-extra-#{`uname -r`} linux-image-extra-virtual docker-engine && service start docker && gpasswd -a adam docker"
end

REPOS_DIR = "#{ENV['HOME']}/repos"

ISOLATED_STACK = "#{ENV['HOME']}/bin/stack"
file ISOLATED_STACK => DOCKER do
  Dir.chdir("#{ENV['HOME']/repos}") do
    sh "git clone git@github.com:ahri/isolated-stack.git"
  end

  ln_s("#{ENV['HOME']}/repos/isolated-stack/stack.sh", STACK)
end

CABAL_STACK = "#{ENV['HOME']}/.cabal/bin/stack"
file CABAL_STACK do
  sh "sudo apt install -y ghc cabal-install"
  Dir.chdir(REPOS_DIR) do
    sh "git clone https://github.com/commercialhaskell/stack"
    Dir.chdir("#{REPOS_DIR}/stack}") do
      sh "cabal update && cabal install"
    end
  end
  sh "sudo apt remove -y ghc cabal-install"
end

APT_STACK = "/usr/bin/stack"
file APT_STACK do
  sh "sudo apt install -y haskell-stack"
end

HASKELLSTACKORG_STACK = "/usr/local/bin/stack"
file HASKELLSTACKORG_STACK do
  sh "curl -sSL https://get.haskellstack.org/ | sh"
end

STACK = HASKELLSTACKORG_STACK
desc "Install Haskell, via Stack"
task :haskell => STACK do
  sh "#{STACK} setup"
  sh "#{STACK} install ghc-mod hoogle hlint stylish-haskell hindent apply-refact"
  sh "#{ENV['HOME']}/.local/bin/hoogle generate"
end

ELM = "#{ENV['HOME']}/bin/elm"
desc "Install Elm"
task :elm => ELM
file ELM => DOCKER do
  Dir.chdir("#{ENV['HOME']/repos}") do
    sh "git clone git@github.com:ahri/isolated-elm.git"
  end

  ln_s("#{ENV['HOME']}/repos/isolated-elm/elm.sh", ELM)
end
