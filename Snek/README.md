# Golang Server Guide

## Run Server 

### To run the server you must call the excecutable with the full path (`~/path/to/project/Server/bin/game_server`).
### This will make the Server run on a local host on port 8080.

## Modify Server

### If you wish to edit the server, you must create the new packages containing the changes. 
### In order to do this properly you must move the server onto your `$GOPATH` src folder, or change your `$GOPATH` to your current directory (not recommended).
### Once the project is in the proper directory, you must install each of the packages you modified.
### To do this you must run `go install` on each of the server's folders that contain a package.
### You must run said command on a specific order. The main package must be installed last, given that this will create the excecutable.
### If a package imports any of your own packages from this project, these must be installed first.