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
    rm target
  end

  begin
    ln_sf source, target
  rescue NotImplementedError
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

desc "Install docker"
task :docker do
  sh "sudo apt update && sudo apt install -y apt-transport-https ca-certificates && apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D"
  File.open("/etc/apt/sources.list.d/docker.list", "w") do |fp|
    fp.puts "deb https://apt.dockerproject.org/repo ubuntu-xenial main"
  end
  sh "sudo apt update && sudo apt install -y linux-image-extra-#{`uname -r`} linux-image-extra-virtual docker-engine && service start docker && gpasswd -a adam docker"
end

desc "Install Haskell, via Stack"
task :haskell do
  sh "curl -sSL https://get.haskellstack.org/ | sh && stack setup"
  Dir.chdir("/tmp") do
    sh "stack new stacksetup simple"
    Dir.chdir("stacksetup") do
      sh "stack build"
    end

    rm_rf "stacksetup"
  end
end

desc "Install NodeJS"
task :nodejs do
  sh "curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash - && sudo apt install -y nodejs"
end

desc "Install Elm"
task :elm => :nodejs do
  sh "sudo npm install -g elm elm-oracle elm-test"
end
