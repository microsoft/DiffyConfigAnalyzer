dotnet publish -p:PublishSingleFile=true -r linux-x64 -c Release --self-contained true -p:PublishTrimmed=true .\src\Diffy.Cli\
dotnet publish -p:PublishSingleFile=true -r win-x64 -c Release --self-contained true -p:PublishTrimmed=true .\src\Diffy.Cli\
New-Item -ItemType Directory -Path ".\build" -ErrorAction SilentlyContinue
Copy-Item -Path .\src\Diffy.Cli\bin\Release\net6.0\linux-x64\publish\Diffy.Cli -Destination .\build\diffy
Copy-Item -Path .\src\Diffy.Cli\bin\Release\net6.0\win-x64\publish\Diffy.Cli.exe -Destination .\build\diffy.exe