// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing 

// Directories
let buildDir  = "./build/"
let deployDir = "./deploy/"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

Target "Test" (fun _ ->
    !! (buildDir + "/06FiniteTrees.dll")
        |> NUnit3 (fun p ->
            { p with
                ToolPath = "packages/NUnit.ConsoleRunner/tools/nunit3-console.exe";
                (* 
                    See:https://github.com/nunit/nunit/issues/1834 for why option is set
                    SOE happens due to test for tail recursiveness
                *)
                ProcessModel=SingleProcessModel })
)


// Build order
"Clean"
  ==> "Build"
  ==> "Test"

// start build
RunTargetOrDefault "Test"
