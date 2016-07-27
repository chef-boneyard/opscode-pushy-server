class KeyCache
  def self.cached_keys
    @cached_keys ||= {}
  end

  def self.get(name)
    cached_keys[name]
  end

  def self.put(name, key)
    cached_keys[name] = key
  end

  def self.to_s
    "KeyCache[known_keys: #{cached_keys.keys}]"
  end

  def self.clean
    cached_keys.each do |name, key|
      puts "Cleaning key for #{name} (#{key.path})"
      key.close
      key.unlink
    end
    @cached_keys = {}
  end
end
