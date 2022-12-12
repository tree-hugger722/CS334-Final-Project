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
    
    [<TestMethod>]
    member this.testWarmSpringSaladWithNuts () =
        let input = "warm spring salad with nuts"
        let output = prettyprint (evaluate input)

//put each line of string output as an element in the array
        let output_substring = output.Split 'n'
//get the array length (# of lines)
        let length = output_substring.Length

//recipe should have six lines, fail test otherwise
        if length = 6 then
            Assert.IsTrue(true)
        else   
            Assert.IsTrue(false)




    

        