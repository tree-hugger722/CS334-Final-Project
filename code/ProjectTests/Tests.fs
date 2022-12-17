namespace ProjectTests
open Parser
open Combinator
open Evaluator
open System
open DataReader
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
    (*No attributes, no exceptions*)
    [<TestMethod>]
    member this.testSummerSalad () =
        let input = "summer salad"
        let expected = Recipe(NoAttribute, Dish (Summer, Salad, NoException))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    
     (* Attribute and exceptions *)
    [<TestMethod>]
    member this.testWarmWinterSalad () =
        let input = "warm winter salad"
        let expected = Recipe(AttributeOne Warm, Dish (Winter, Salad, NoException))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    
    (* Attribute and exceptions *)
    [<TestMethod>]
    member this.testColdSpringSalad () =
        let input = "cold spring salad"
        let expected = Recipe(AttributeOne Cold, Dish (Spring, Salad, NoException))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)

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
    
    (* Multiple Exceptions, no attribute *)
    [<TestMethod>]
    member this.testFallSaladWithCheeseRadishNoNuts () =
        let input = "fall salad with cheese, (Radish)"
        let expected = Recipe(NoAttribute, Dish (Fall, Salad, SoftCore(Include, [Cat Cheese; StrName "radish"])))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    
     (* Attribute and exceptions *)
    [<TestMethod>]
    member this.testWarmFallSaladWithCheeseRadishNoNuts () =
        let input = "warm fall salad with cheese, (Lettuce) and without nuts"
        let expected = Recipe(AttributeOne Warm, Dish (Fall, Salad, HardCore (SoftCore(Include, [Cat Cheese; StrName "lettuce"]), SoftCore(Exclude, [Cat Nut]))))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    
     (* Attribute and exceptions *)
    [<TestMethod>]
    member this.testColdSpringSaladWithoutButternutSquash () =
        let input = "warm fall salad without (Butternut Squash)"
        let expected = Recipe(AttributeOne Warm, Dish (Fall, Salad, SoftCore(Exclude, [StrName "butternut squash"])))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    
    (* Attribute and exceptions *)
    [<TestMethod>]
    member this.testColdSpringSaladWithoutButternutSquashCheese () =
        let input = "warm fall salad without (Butternut Squash), (Aged Cheddar)"
        let expected = Recipe(AttributeOne Warm, Dish (Fall, Salad, SoftCore (Exclude, [StrName "butternut squash"; StrName "aged cheddar"])))
        let result = parse input
        match result with 
        |Some ast ->
            Assert.AreEqual(expected, ast)
        |None ->
            Assert.IsTrue(false)
    

    (* Incorrect Ingredient Syntax *)
    [<TestMethod>]
    member this.testIncorrectIngredient () =
        let input = "fall salad with Butternut Squash"
        let result = parse input
        Assert.ThrowsException<Exception>((fun _ -> parse input |> ignore), "usage: please input in correct formula. Some examples are: 
        cold spring salad
        warm winter salad with nuts, (artichokes)")

    (* Incorrect spelling *)
    [<TestMethod>]
    member this.misspellInput () =
        let input = "fallg salad"
        let result = parse input
        Assert.ThrowsException<Exception>((fun _ -> parse input |> ignore), "usage: please input in correct formula. Some examples are: 
        cold spring salad
        warm winter salad with nuts, (artichokes)")


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
        let input = Some (Recipe(NoAttribute, Dish (Spring, Salad, SoftCore (Include, [Cat Vegetable]))))
        let output = eval input
        let output_lines = output.Split '\n'
        Assert.AreEqual(cold_salad_len,output_lines.Length)

        let input2 = SoftCore (Include, [Cat Vegetable])
        let (included, excluded) = (processExceptions input2)
        Assert.AreEqual<Ingredient list>([], included)
        Assert.AreEqual<Ingredient list>([], excluded)

    (* Single Exception (excluding a category) *)
    [<TestMethod>]
    member this.testExcludeSingleCategory () =
        let input = Some (Recipe(NoAttribute, Dish (Spring, Salad, SoftCore (Exclude, [Cat Vegetable]))))
        let output = eval input
        let output_lines = output.Split '\n'

        // Check that there are two fewer ingredients than normal
        Assert.AreEqual(cold_salad_len-2, output_lines.Length)

        let input2 = SoftCore (Exclude, [Cat Vegetable])
        let (included, excluded) = (processExceptions input2)
        Assert.AreEqual(([]:Ingredient list), included)
        for ingredient in excluded do
            Assert.AreEqual(Vegetable, ingredient.Category)

    (* Double Exception (two ingredients) *)
    [<TestMethod>]
    member this.testIncludeTwoIngredients () =
        let input = Some (Recipe(NoAttribute, Dish (Fall, Salad, SoftCore (Include, [StrName "butternut squash"; StrName "beet"]))))
        let output = eval input
        let output_lines = output.Split '\n'

        // Check that there are two fewer ingredients than normal
        Assert.AreEqual(cold_salad_len, output_lines.Length)

        let except = SoftCore (Include, [StrName "butternut squash"; StrName "beet"])
        let output = (saladGen Fall except Cold)
        let butternut = List.find (fun x -> x.Name = "Butternut Squash") output
        let beet = List.find (fun x -> x.Name = "Beet") output
        Assert.AreEqual(butternut, {Name="Butternut Squash"; Quantity=5.000M; Unit=Ounce; Season_List=[Winter;Fall]; Category=Vegetable})
        Assert.AreEqual(beet, {Name="Beet"; Quantity=4.000M; Unit=Ounce; Season_List=[Summer;Winter;Fall]; Category=Vegetable})

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

