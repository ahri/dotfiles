foreach($ext in Get-Content .\extensions.list) {
    code --install-extension $ext
}