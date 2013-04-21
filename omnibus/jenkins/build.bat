SETLOCAL

:loop
IF "%1"=="" GOTO :continue
IF "%1"=="-p" (
  SET omnibus_project=%2
  SHIFT
)
SHIFT
GOTO :loop
:continue

if "%omnibus_project%"=="" (
  ECHO Usage: build [-p PROJECT_NAME]
  EXIT /B 1
)

rem # These must match what is defined in the windows-dna.json
rem # referenced below
set BUILD_WORKSPACE=c:\opscode_pushy_build
set OMNIBUS_RUBY_CACHE=c:\omnibus-ruby-cache-pushy

rem # IF NOT EXIST jenkins\chef-solo\cache mkdir jenkins\chef-solo\cache

IF "%CLEAN%"=="true" (
  rmdir /Q /S %BUILD_WORKSPACE%
  rmdir /Q /S %OMNIBUS_RUBY_CACHE%
  rmdir /Q /S .\pkg
)

call bundle install || GOTO :error

rem # ensure berkshelf is installed
where /q berks
IF NOT %ERRORLEVEL% == 0 (
  call gem install berkshelf --no-ri --no-rdoc
)

rem # install omnibus cookbook and dependencies
call berks install --path=vendor/cookbooks

call chef-solo -c .\jenkins\solo.rb -j .\jenkins\dna-windows.json -l debug || GOTO :error

call copy /Y omnibus.rb.example.windows omnibus.rb || GOTO :error

rem # we're guaranteed to have the correct ruby installed into C:\Ruby193 from chef-solo cookbooks
rem # bundle install from here now too
set PATH=C:\Ruby193\bin;%PATH%
rem # ensure the installed certificate authority is loaded
set SSL_CERT_FILE=C:\Ruby193\ssl\certs\cacert.pem
call bundle install || GOTO :error

call bundle exec omnibus build project %omnibus_project%-windows || GOTO :error

GOTO :EOF

:error
ECHO Failed with error level %errorlevel%

ENDLOCAL
