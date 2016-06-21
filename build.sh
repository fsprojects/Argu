#!/bin/bash
if [ "X$OS" = "XWindows_NT" ] ; then
  # use .Net

  .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  .paket/paket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
else

  # use mono
  mono .paket/paket.bootstrapper.exe
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono .paket/paket.exe restore -v
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi
  mono packages/build/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx 
fi