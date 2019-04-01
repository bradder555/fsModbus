module Tests

open Hopac
open Expecto
open Util

let tests =
  testList "." ([ // List is alphabetical order
    Test.Deserialization.tests
    Test.Serialization.tests
    Test.Util.tests
  ] |> List.rev) // the sequenced summary output is this list reversed

// test util
let runTests = 
  job {
    // i run sequenced simply so the output summary is in order,
    // this means tests will run slightly longer, but i doubt it will be 
    // bothersome
    return runTestsWithArgs defaultConfig [|"--summary"; "--sequenced";|] tests
  }