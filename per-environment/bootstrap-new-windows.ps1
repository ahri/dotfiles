# iex (new-object net.webclient).downloadstring('https://raw.githubusercontent.com/ahri/dotfiles/master/per-environment/bootstrap-new-windows.ps1')

runas /noprofile /user:Administrator Get-ItemProperty HKLM:\SYSTEM\CurrentControlSet\Enum\HID\*\*\Device` Parameters FlipFlopWheel -EA 0 | ForEach-Object { Set-ItemProperty $_.PSPath FlipFlopWheel 1 }
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
scoop bucket add extras
scoop install 7zip vim openssh git grep touch wget gawk concfg pshazz stack python ruby nodejs msys2
[environment]::setenvironmentvariable('GIT_SSH', (resolve-path (scoop which ssh)), 'USER')
gem install rake
stack setup
concfg import flat small

mkdir $ENV:UserProfile\.ssh
cd $ENV:UserProfile\.ssh
ssh-keygen -b 4096 -f id_rsa
cat id_rsa.pub
Read-Host -Prompt "Add key to github then hit [enter]"

mkdir $ENV:UserProfile\repos
cd $ENV:UserProfile\repos
git clone git@github.com:ahri/dotfiles.git
git config --global user.email "adam@ahri.net"
git config --global user.name "Adam Piper"
cd dotfiles
rake
