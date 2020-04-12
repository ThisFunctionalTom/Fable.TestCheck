module Tests

open Fable.Mocha
open Fable.SimpleJson

[<AutoOpen>]
module TestCheck =
    open Fable.Core
    
    type Array<'T> = System.Collections.Generic.IList<'T>

    type ValueGenerator<'T> = interface end

    type SizeOptions =
        abstract size: float option
        abstract min: float option
        abstract max: float option

    type IGen =
        abstract any: ValueGenerator<obj>
        abstract primitive: ValueGenerator<obj>
        abstract boolean: ValueGenerator<bool>
        abstract ``null``: ValueGenerator<obj>
        abstract undefined: ValueGenerator<obj>
        abstract NaN: ValueGenerator<float>
        
        abstract number: ValueGenerator<float>
        abstract posNumber: ValueGenerator<float>
        abstract negNumber: ValueGenerator<float>
        abstract numberWithin: min:float * max:float -> ValueGenerator<float>
        abstract int: ValueGenerator<int>
        abstract posInt: ValueGenerator<int>
        abstract negInt: ValueGenerator<int>
        abstract sPosInt: ValueGenerator<int>
        abstract sNegInt: ValueGenerator<int>
        abstract intWithin: min:int * max:int -> ValueGenerator<int>

        abstract string: ValueGenerator<string>
        abstract asciiString: ValueGenerator<string>
        abstract alphaNumString: ValueGenerator<string>
        abstract substring: original:string -> ValueGenerator<string>
        abstract char: ValueGenerator<char>
        abstract asciiChar: ValueGenerator<char>
        abstract alphaNumChar: ValueGenerator<char>

        abstract array: valueGen:ValueGenerator<'T> * ?sizeOptions:SizeOptions -> ValueGenerator<'T array>


    type Property<'TArgs> =
        interface end

    type ITestCheck =
        abstract sample: gen: ValueGenerator<'T> * ?numValues: int -> Array<'T>
        abstract sampleOne: gen: ValueGenerator<'T> * ?size: int -> 'T
        abstract property: genA: ValueGenerator<'A> * f: ('A -> U2<bool, unit>) -> Property<'A>
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * f: ('A -> 'B -> U2<bool, unit>) -> Property<'A * 'B>
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * f: ('A -> 'B -> 'C -> U2<bool, unit>) -> Property<'A * 'B * 'C>
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * f: ('A -> 'B -> 'C -> 'D -> U2<bool, unit>) -> Property<'A * 'B * 'C * 'D>
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * genE: ValueGenerator<'E> * f: ('A -> 'B -> 'C -> 'D -> 'E -> U2<bool, unit>) -> Property<'A * 'B * 'C * 'D * 'E>

        abstract gen: IGen with get

    [<ImportAll("testcheck")>]
    let tc: ITestCheck = jsNative

let testcheckTests =
    testList "TestCheck tests" [ 
        testCase "sampleOne generates values smaller then given size" <| fun _ ->
            let values = Seq.init 1024 (fun _ -> tc.sampleOne (tc.gen.int, 30))
            Expect.isTrue (Seq.max values <= 30) "Should actually be smaller then 30"

        testCase "sample generates int smaller then given size" <| fun _ ->
            let values = tc.sample (tc.gen.int, 200)
            Expect.isTrue (Seq.max values <= 200) "Should actually be smaller then 200"

        testCase "gen.any: print generated values" <| fun _ ->
            let values = tc.sample (tc.gen.any, 20)
            // values
            // |> Seq.iter (Json.stringify >> printfn "%s")
            Expect.pass () "Just show the values"

        testCase "gen.null generates just null values" <| fun _ ->
            let values = tc.sample (tc.gen.``null``, 20)
            Expect.isTrue (values |> Seq.forall isNull) "All values should be null"

        testCase "gen.numberWithin generates numbers withing given range" <| fun _ ->
            let values = tc.sample (tc.gen.numberWithin(-10.5, 10.5), 20)
            Expect.isTrue (Seq.max values <= 10.5) "All values should be smaller or equal given max value"
            Expect.isTrue (Seq.min values >= -10.5) "All values should be greater or equal given min value"

        testCase "gen.intWithin generates ints withing given range" <| fun _ ->
            let values = tc.sample (tc.gen.intWithin(-10, 10), 20)
            Expect.isTrue (Seq.max values <= 10) "All values should be smaller or equal given max value"
            Expect.isTrue (Seq.min values >= -10) "All values should be greater or equal given min value"

        testCase "gen.string generates strings with length smaller then size" <| fun _ ->
            let values = List.init 1024 (fun _ -> tc.sampleOne (tc.gen.string, 20))
            Expect.isTrue (List.max (List.map String.length values) <= 20) "All values should be smaller or equal given max value"

        testCase "gen.char generates char or string?" <| fun _ ->
            Seq.init 1024 (fun _ -> tc.sampleOne (tc.gen.char, 20))
            |> Seq.iter (fun c -> Expect.isTrue (c.GetType().Name = "Char") "Should be char")

        testCase "gen.array generates array without options" <| fun _ ->
            let someArray = tc.sampleOne (tc.gen.array(tc.gen.int), 20)
            Expect.isTrue (someArray.Length <= 20) "Should be shorter or equal 20 elements"

        testCase "gen.array generates array with size options" <| fun _ ->
            let sizeOptions = 
                { new SizeOptions with 
                        member __.size = Some 1020.0
                        member __.min = None
                        member __.max = None }
            let someArray = tc.sampleOne (tc.gen.array(tc.gen.int, sizeOptions), 20)
            Expect.isTrue (someArray.Length <= 1024) "Should be shorter or equal 1024 elements"
            
    ]
        

let allTests = testList "All" [
    testcheckTests
]

[<EntryPoint>]
let main (args: string[]) = Mocha.runTests allTests