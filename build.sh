#!/bin/bash

if [ ! -f packages/FAKE/tools/Fake.exe ]; then
  mono .nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi
if [ ! -f packages/SourceLink.Fake/tools/SourceLink.fsx ]; then
  mono .nuget/NuGet.exe install SourceLink.Fake -OutputDirectory packages -ExcludeVersion
fi

run_fake() {
    configuration=$1

    if [ $configuration = "--publish-nuget" ]; then
        configuration=Release
        publish_nuget=true
    else
        publish_nuget=false
    fi

    echo "Building $configuration..."
    mono packages/FAKE/tools/FAKE.exe build.fsx -d:MONO Configuration="$configuration" PublishNuget=$publish_nuget
}

if [ $# -eq 0 ]; then
    run_fake "Release"
elif [ $# -eq 1 ]; then
    run_fake $1
else
    for config in $* ; do
        run_fake "$config"
    done
fi
