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
      <string>/usr/local/bin/emacs</string>
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

desc 'install the emacs configuration into the user\'s home directory'
task :install do
  dir = File.expand_path(File.dirname(__FILE__))
  emacsd = '.emacs.d'
  link = lambda { system %Q{ln -s #{dir} "$HOME/#{emacsd}"}; puts '.emacs.d linked' }
  if File.exist?(File.join(ENV['HOME'], emacsd))
    print "overwrite ~/#{emacsd}? [yn] "
    case $stdin.gets.chomp
    when 'y'
      system %Q{rm -rf "$HOME/#{emacsd}"}
      link.call
    else
      puts "not linking emacs.d"
    end
  else
    link.call
  end
end
