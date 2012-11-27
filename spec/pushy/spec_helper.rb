$:.unshift File.expand_path("../../lib", __FILE__)
$:.unshift File.expand_path("../..", __FILE__)

require 'bundler'
Bundler.require(:default, :test)

#require 'pushy-client'

require 'tmpdir'
require 'tempfile'

require 'pushy/support/end_to_end_util'

WATCH = lambda { |x| puts x } unless defined?(WATCH)

# Load everything from spec/support
# Do not change the gsub.
Dir["pushy/support/**/*.rb"].map { |f| f.gsub(%r{.rb$}, '') }.each { |f| require f }
