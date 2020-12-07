#!/bin/sh
export version=$1
export key=$2
dotnet pack -c release -p:PackageVersion=$version
dotnet nuget push ./Library/bin/Release/fsModbus.$version.nupkg -s https://api.nuget.org/v3/index.json -k $key
