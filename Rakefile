# The MIT License
#
# Copyright (c) Zach Holman, http://zachholman.com
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
#
# Made some edits to deal with the gitconfig file within the install task, but 
# bulk of this Rakefile is credited to Zach Holman (hence the above license).
#
#   -Andrew, 2011-04-18 19:47:36 

require 'rake'

desc "Hook our dotfiles into system-standard positions."
task :install => [:generate_gitconfig_from_template] do
  linkables = Dir.glob('*/**{.symlink}')

  skip_all = false
  overwrite_all = false
  backup_all = false

  linkables.each do |linkable|
    overwrite = false
    backup = false

    file = linkable.split('/').last.split('.symlink').last
    target = "#{ENV["HOME"]}/.#{file}"

    if File.exists?(target) || File.symlink?(target)
      unless skip_all || overwrite_all || backup_all
        puts "File already exists: #{target}, what do you want to do? [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all"
        case STDIN.gets.chomp
        when 'o' then overwrite = true
        when 'b' then backup = true
        when 'O' then overwrite_all = true
        when 'B' then backup_all = true
        when 'S' then skip_all = true
        end
      end
      FileUtils.rm_rf(target) if overwrite || overwrite_all
      `mv "$HOME/.#{file}" "$HOME/.#{file}.backup"` if backup || backup_all
    end
    `ln -s "$PWD/#{linkable}" "#{target}"`
  end
end
task :default => 'install'

desc "generate a gitconfig file from the template based on your input"
task :generate_gitconfig_from_template do
  gitconfig_name = 'git/gitconfig.symlink'
  repl = {}
  puts "\nGenerating gitconfig"
  print("Your Name: "); STDOUT.flush; repl['__USER_NAME__'] = STDIN.gets.chomp
  print("Your Email: "); STDOUT.flush; repl['__USER_EMAIL__'] = STDIN.gets.chomp
  print("GitHub Username: "); STDOUT.flush; repl['__GITHUB_USER__'] = STDIN.gets.chomp
  print("GitHub API Token: "); STDOUT.flush; repl['__GITHUB_TOKEN__'] = STDIN.gets.chomp
  temp = IO.read('git/gitconfig.template')
  repl.each { |k,v| temp.gsub!(k,v) }
  File.new(gitconfig_name, File::WRONLY|File::TRUNC|File::CREAT).puts temp
end
