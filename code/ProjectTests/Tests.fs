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

    let cold_salad_len = 7
    let warm_salad_len = 9

    (***** PARSER TESTS *****)
    (* Single Exception, Attribute *)
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

    (* Incorrect Ingredient Syntax *)
    [<TestMethod>]
    member this.testIncorrectIngredient () =
        // don't test the specific error output, but test that it fails
        0


    (***** EVALUATOR TESTS *****)  
    (* Single Exception (including an ingredient) *)
    [<TestMethod>]
    member this.testIncludeSingleIngredient () =
        let input = Some (Recipe(NoAttribute, Dish (Spring, Salad, SoftCore (Include, [StrName "swiss chard"]))))
        let output = eval input
        let output_lines = output.Split '\n'

        Assert.IsTrue(output.Contains "Swiss Chard")
        Assert.AreEqual(cold_salad_len, output_lines.Length)


    (* Single Exception (excluding an ingredient) *)
    [<TestMethod>]
    member this.testExcludeSingleIngredient () =
        let input = Some (Recipe(NoAttribute, Dish (Spring, Salad, SoftCore (Exclude, [StrName "swiss chard"]))))
        let output = eval input
        let output_lines = output.Split '\n'

        Assert.IsFalse(output.Contains "Swiss Chard")
        Assert.AreEqual(cold_salad_len, output_lines.Length)

    (* Single Exception (including a category) *)
    [<TestMethod>]
    member this.testIncludeSingleCategory () =
        0

    (* Single Exception (excluding a category) *)
    [<TestMethod>]
    member this.testExcludeSingleCategory () =
        let input = Some (Recipe(NoAttribute, Dish (Spring, Salad, SoftCore (Exclude, [Cat Vegetable]))))
        let output = eval input
        let output_lines = output.Split '\n'

        // Check that none of the ingredients belong to the vegetable category
        // NOT DONE WITH THIS!

        // Check that there are two fewer ingredients than normal
        Assert.AreEqual(cold_salad_len-2, output_lines.Length)

    (* Double Exception (two ingredients) *)
    (* Double Exception (two categories) *)
    (* Double Exception (one category, one ingredient) *)
    (* No Exception *)


    (***** END-TO-END TESTS *****) 

    (* Tests that cold salads excluding a single category have the correct length *)
    [<TestMethod>]
    member this.testColdSpringSaladWithoutNuts () =
        let input = "cold spring salad without nuts"
        let output = eval (parse input)

        //put each line of string output as an element in the array
        let output_substring = output.Split '\n'
        //get the array length (# of lines)
        let length = output_substring.Length
        printf "length: %i" length

        //recipe should have six lines + 1 (including extra return at the end), fail test otherwise
        Assert.AreEqual(cold_salad_len-1, length)

    (* Tests that warm salads excluding a single category have the correct length *)
    [<TestMethod>]
    member this.testWarmSpringSaladWithoutNuts () =
        let input = "warm spring salad without nuts"
        let output = eval (parse input)

        //put each line of string output as an element in the array
        let output_substring = output.Split '\n'
        //get the array length (# of lines)
        let length = output_substring.Length
        printf "length: %i" length

        //recipe should have six lines, fail test otherwise
        Assert.AreEqual(warm_salad_len, length)




    

        