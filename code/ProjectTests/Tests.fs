namespace ProjectTests
open Parser
open Combinator
open Evaluator
open System
open System.IO
open System.Runtime.Serialization.Formatters.Binary
open Microsoft.FSharp.Core

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.testWarmSpringSaladWithCheese () =
        let input = "warm spring salad with cheese"
        let expected = Recipe(AttributeOne Warm, Dish (Spring, Salad, SoftCore (Include, [Cat Cheese])))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    

(*
    Full run through test method: tests for a 6 line recipe
 *)

    [<TestMethod>]
    member this.testWarmSpringSaladWithNuts () =
        let input = "warm spring salad without nuts"
        let output = eval (parse input)

//put each line of string output as an element in the array
        let output_substring = output.Split '\n'
//get the array length (# of lines)
        let length = output_substring.Length
        printf "length: %i" length

//recipe should have six lines + 1 (including extra return at the end), fail test otherwise
        Assert.AreEqual(7, length)




    

        