#!/bin/bash

dotnet publish -p:PublishSingleFile=true -r linux-x64 -c Release --self-contained true -p:PublishTrimmed=true ./src/Diffy.Cli/
dotnet publish -p:PublishSingleFile=true -r win-x64 -c Release --self-contained true -p:PublishTrimmed=true ./src/Diffy.Cli/
mkdir -p ./build
cp ./src/Diffy.Cli/bin/Release/net6.0/linux-x64/publish/Diffy.Cli ./build/diffy
cp ./src/Diffy.Cli/bin/Release/net6.0/win-x64/publish/Diffy.Cli.exe ./build/diffy.exe