runas /noprofile /user:Administrator Get-ItemProperty HKLM:\SYSTEM\CurrentControlSet\Enum\HID\*\*\Device` Parameters FlipFlopWheel -EA 0 | ForEach-Object { Set-ItemProperty $_.PSPath FlipFlopWheel 1 }
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
scoop bucket add extras
scoop install 7zip vim openssh git grep touch wget gawk concfg pshazz stack python ruby nodejs msys2
[environment]::setenvironmentvariable('GIT_SSH', (resolve-path (scoop which ssh)), 'USER')
gem install rake
stack setup
concfg import flat small

mkdir %USERPROFILE%\.ssh
cd %USERPROFILE%\.ssh
ssh-keygen -b 4096 -f id_rsa
cat id_rsa.pub
Read-Host -Prompt 'Add to github...'

mkdir %USERPROFILE%\repos
cd %USERPROFILE%\repos
git clone git@github.com:ahri/dotfiles.git
cd dotfiles
rake
