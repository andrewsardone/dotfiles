# Copyright (c) 2008 Ryan Bates
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# ---
#
# Made some edits to deal with the gitconfig file within the install task
#
#   -Andrew

require 'rake'

desc "install the dot files into user's home directory"
task :install => [:generate_gitconfig_from_template] do
  replace_all = false
  Dir['*'].each do |file|
    # edit
    next if %w[Rakefile README.md symlinker.sh terminal_profiles gitconfig.template].include? file

    if File.exist?(File.join(ENV['HOME'], ".#{file}"))
      if replace_all
        replace_file(file)
      else
        print "overwrite ~/.#{file}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/.#{file}"
        end
      end
    else
      link_file(file)
    end
  end
end

desc "generate a gitconfig file from the template based on your input"
task :generate_gitconfig_from_template do
  gitconfig_name = 'gitconfig'
  repl = {}
  puts "\nGenerating gitconfig"
  print("Your Name: "); STDOUT.flush; repl['__USER_NAME__'] = STDIN.gets.chomp
  print("Your Email: "); STDOUT.flush; repl['__USER_EMAIL__'] = STDIN.gets.chomp
  print("GitHub Username: "); STDOUT.flush; repl['__GITHUB_USER__'] = STDIN.gets.chomp
  print("GitHub API Token: "); STDOUT.flush; repl['__GITHUB_TOKEN__'] = STDIN.gets.chomp
  temp = IO.read('gitconfig.template')
  repl.each { |k,v| temp.gsub!(k,v) }
  File.new(gitconfig_name, File::WRONLY|File::TRUNC|File::CREAT).puts temp
end

def replace_file(file)
  system %Q{rm "$HOME/.#{file}"}
  link_file(file)
end

def link_file(file)
  puts "linking ~/.#{file}"
  system %Q{ln -s "$PWD/#{file}" "$HOME/.#{file}"}
end
