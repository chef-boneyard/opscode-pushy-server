Gem::Specification.new do |s|
  s.name        = 'oc-pushy-pedant'
  s.version     = '2.0.0'
  s.date        = '2015-06-29'
  s.summary     = "API tests for Opscode's Private Chef Push Server"
  s.description = "API tests for Opscode's Private Chef Push Server; requires Pedant to actually run"
  s.authors     = ["Doug Triggs", "John Keiser", "Mark Anderson"]
  s.email       = 'maintainers@chef.io'
  s.files       = Dir['spec/**/*_spec.rb'] + Dir['lib/**/*.rb']
  s.homepage    = 'http://opscode.com'
  s.require_paths = ['spec', 'lib']

  s.add_dependency "httpclient"
  s.add_dependency "typhoeus"
end
