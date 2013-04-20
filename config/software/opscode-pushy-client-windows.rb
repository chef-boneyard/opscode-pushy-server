#
# Copyright:: Copyright (c) 2013 Opscode, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

name "opscode-pushy-client-windows"
version "master"

dependency "bundler"

# TODO - use public GIT URL when repo made public
source :git => "git@github.com:opscode/opscode-pushy-client.git"

relative_path "opscode-pushy-client"


env =
  case platform
  when "solaris2"
    if Omnibus.config.solaris_compiler == "studio"
    {
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include"
    }
    elsif Omnibus.config.solaris_compiler == "gcc"
    {
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
      "LD_OPTIONS" => "-R#{install_dir}/embedded/lib"
    }
    else
      raise "Sorry, #{Omnibus.config.solaris_compiler} is not a valid compiler selection."
    end
  else
    {
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "LDFLAGS" => "-Wl,-rpath #{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include"
    }
  end

build do
  gem ["install",
       "zmq",
       "-n #{install_dir}/bin",
       "--no-rdoc --no-ri",
       "--",       
       "--with-zmq-dir=#{install_dir}/embedded/lib/zeromq",
       "--with-zmq-lib=#{install_dir}/embedded/lib/zeromq/bin"
      ].join(" "), :env => env

  auxiliary_gems = ["uuidtools rdp-ruby-wmi windows-api windows-pr win32-dir win32-event win32-mutex win32-process"]

  gem ["install",
       auxiliary_gems.join(" "),
       "-n #{install_dir}/bin",
       "--no-rdoc --no-ri",
       "--",       
       "--with-zmq-dir=#{install_dir}/embedded/lib/zeromq",
       "--with-zmq-lib=#{install_dir}/embedded/lib/zeromq/bin"
      ].join(" "), :env => env
  
  rake "gem", :env => env  

  gem ["install pkg/opscode-pushy-client*.gem",
       "-n #{install_dir}/bin",
       "--no-rdoc --no-ri"].join(" ")

    # render batch files
  #
  # TODO: 
  #  I'd love to move this out to a top-level 'template' operation in omnibus, but it currently
  #  requires pretty deep inspection of the Rubygems structure of the installed chef and ohai
  #  gems
  #
  block do
    require 'erb'
    require 'rubygems/format'

    batch_template = ERB.new <<EOBATCH
@ECHO OFF
IF NOT "%~f0" == "~f0" GOTO :WinNT
@"%~dp0\\..\\embedded\\bin\\ruby.exe" "%~dp0/<%= @bin %>" %1 %2 %3 %4 %5 %6 %7 %8 %9
GOTO :EOF
:WinNT
@"%~dp0\\..\\embedded\\bin\\ruby.exe" "%~dpn0" %*
EOBATCH

    gem_executables = []
    %w{opscode-pushy-client}.each do |gem|
      gem_file = Dir["#{install_dir}/embedded/**/cache/#{gem}*.gem"].first
      gem_executables << Gem::Format.from_file_by_path(gem_file).spec.executables
    end

    gem_executables.flatten.each do |bin|
      @bin = bin
      File.open("#{install_dir}/bin/#{@bin}.bat", "w") do |f|
        f.puts batch_template.result(binding)
      end
    end
  end

  
end
