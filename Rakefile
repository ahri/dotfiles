HOME = ENV['HOME'] || ENV['USERPROFILE']

BUNDLE_DIR = "#{HOME}/.vim/bundle"
VUNDLE_IDENTIFIER = "#{BUNDLE_DIR}/Vundle.vim/.gitignore"

task :default => [:required_tooling, :dotfiles, :vundle]

task :required_tooling do
  ['vim', 'git'].each do |tool|
    next if has_program? tool

    puts "ERROR: Install #{tool}"
    exit 1
  end
end

task :dotfiles

task :vundle => [VUNDLE_IDENTIFIER]


def multiplatform_symlink(source, target)
  source = File.absolute_path source
  target = File.absolute_path target

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
      # Ignore directories for now
    else
      target = "#{HOME}/#{f}"
      task :dotfiles => target
      file target => f do
        multiplatform_symlink f, target
      end
    end
  end
end

create_dotfiles_tasks

def has_program?(program)
  ENV['PATH'].split(File::PATH_SEPARATOR).any? do |directory|
    File.executable?(File.join(directory, program.to_s)) or File.executable?(File.join(directory, "#{program.to_s}.exe"))
  end
end

file VUNDLE_IDENTIFIER => [BUNDLE_DIR] do
  `git clone https://github.com/gmarik/Vundle.vim.git "#{BUNDLE_DIR}/Vundle.vim"`
  `vim +PluginInstall +qall`
end

directory BUNDLE_DIR
