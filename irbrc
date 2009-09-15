require 'rubygems'
require 'wirble'
require 'hirb'

Wirble.init
Wirble.colorize

Hirb.enable

class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end