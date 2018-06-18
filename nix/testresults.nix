{ curl, reflexPlatform, testdriver, testserver, phantomjs }:

reflexPlatform.nixpkgs.pkgs.runCommand "runWebdriveTest.sh" {} ''
  echo About to phantom
  ${phantomjs}/bin/phantomjs --webdriver=127.0.0.1:4444 &
  sleep 3

  echo About to server
  cd ${testserver} && ./back -q --no-access-log --no-error-log &
  sleep 3

  ${curl}/bin/curl localhost:8000
  ${curl}/bin/curl localhost:8000/runmain.js

  echo About to testdrive
  ${testdriver}/bin/spec
  echo Done
  mkdir $out
  echo Done > $out/out.txt
  trap "exit" INT TERM
  trap "kill 0" EXIT
  ''