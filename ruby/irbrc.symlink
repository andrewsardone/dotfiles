begin
  require 'wirble'

  Wirble.init
  Wirble.colorize

rescue LoadError => err
  warn "Couldn't load Wirble: #{err}"
end

begin
  require 'hirb'

  Hirb.enable
rescue LoadError => err
  warn "Couldn't load Hirb: #{err}"
end

class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end
