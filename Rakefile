require 'rake'

task :daemonize => [:write_plist, :load]

task :check_sudo do
  unless `whoami`.chop == 'root'
    puts 'You need to run via sudo'
    exit
  end
end

desc 'write a launchd plist to start an emacs daemon'
task :write_plist do
  @plist_path = ENV['HOME'] + '/Library/LaunchAgents/gnu.emacs.daemon.plist'
  @plist_content = <<-EOS
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" 
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd"> 
<plist version="1.0">
  <dict> 
    <key>Label</key>
    <string>gnu.emacs.daemon</string>
    <key>ProgramArguments</key>
    <array>
      <string>/usr/local/homebrew/bin/emacs</string>
      <string>--daemon</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
    <key>ServiceDescription</key>
    <string>Gnu Emacs Daemon</string>
    <key>UserName</key>
    <string>andrew</string>
  </dict>
</plist>
EOS
  File.open(@plist_path, 'w') {|f| f.write(@plist_content) }
  puts "\tWriting #{@plist_path}"
end

desc 'load the plist into launchd'
task :load do
  puts "\tRunning `launchctl load -w #{@plist_path}`"
  `launchctl load -w #{@plist_path}`
end
