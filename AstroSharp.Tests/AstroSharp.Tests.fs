module AstroSharp.Tests

open Expecto
open AstroSharp.Core.Gravitation

let gravitationTests = 
    testList "Gravitation Tests" [
        test "0 Distance Between Bodies" {
            let noneResult = getGravitationForce 10.0 10.0 0.0
            Expect.equal noneResult None "Result is None since the distance is 0"
        }

        test "Mass of Body 1 is Negative" {
            let noneResult = getGravitationForce -1.0 10.0 0.0
            Expect.equal noneResult None "Result is None since the mass of the 1st body is negative"
        }

        test "Mass of Body 2 is Negative" {
            let noneResult = getGravitationForce 10.0 -1.0 0.0
            Expect.equal noneResult None "Result is None since the mass of the 2nd body is negative"
        }

        test "Gravitation Smoke Test" {
            let someResult = getGravitationForce 10.0 10.0 1.0
            Expect.isSome someResult "Result has a Value i.e. not None"
            Expect.floatClose Accuracy.medium someResult.Value ( G * 100.0 ) "Result is 100 times G"
        }
    ]

[<EntryPoint>]
let main argv =
    runTestsWithArgs defaultConfig argv gravitationTests 