namespace ProjectTests
open Parser
open Combinator
open Evaluator

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
    

        