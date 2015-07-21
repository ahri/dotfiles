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
  sh "vim -u .vim/installrc.vim +PlugClean! +qall"
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
