module Tests

open Fable.Mocha
open Fable.SimpleJson

module TestCheck =
    open Fable.Core
    
    type Array<'T> = System.Collections.Generic.IList<'T>

    type ValueGenerator<'T> = interface end

    type SizeOptions =
        abstract size: float option
        abstract min: float option
        abstract max: float option

    let size size =
        { new SizeOptions with 
          member __.size = Some size
          member __.min = None
          member __.max = None }

    let within min max =
        { new SizeOptions with 
          member __.size = None
          member __.min = Some min
          member __.max = Some max }

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
        abstract uniqueArray: valueGen:ValueGenerator<'T> * ?uniqueFn: ('T -> int) * ?sizeOptions:SizeOptions -> ValueGenerator<'T array>
        abstract object: valueGen:ValueGenerator<'T> * ?size:int -> ValueGenerator<obj>
        abstract object: keyGen:ValueGenerator<'Key> * valueGen:ValueGenerator<'Value> -> ValueGenerator<obj>
        abstract arrayOrObject: keyGen:ValueGenerator<'T> -> ValueGenerator<U2<'T array, obj>>
        abstract nested: collectionGenFn:(ValueGenerator<'T> -> ValueGenerator<'Collection>) * valueGen:ValueGenerator<'T> -> ValueGenerator<'Collection>
        
        abstract JSON: ValueGenerator<obj>
        abstract JSONValue: ValueGenerator<obj>
        abstract JSONPrimitive: ValueGenerator<U4<string, float, bool, obj>>
        
        abstract ``return``: value:'T -> ValueGenerator<'T>
        abstract deepCopyOf: value:'T -> ValueGenerator<'T>
        abstract sized: genFn:(int -> ValueGenerator<'T>) -> ValueGenerator<'T>

    type IProperty =
        interface end

    type CheckOptions =
        abstract numTests : int
        abstract maxSize : int
        abstract seed: int

    type CheckResult =
        abstract result : bool
        abstract numTests : int
        abstract seed: int
        abstract fail: obj
        abstract shrunk: obj

    type ITestCheck =
        abstract check: property:IProperty * ?options:CheckOptions -> CheckResult
        abstract sample: gen: ValueGenerator<'T> * ?numValues: int -> Array<'T>
        abstract sampleOne: gen: ValueGenerator<'T> * ?size: int -> 'T
        abstract property: genA: ValueGenerator<'A> * f: ('A -> U2<bool, unit>) -> IProperty
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * f: ('A -> 'B -> U2<bool, unit>) -> IProperty
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * f: ('A -> 'B -> 'C -> U2<bool, unit>) -> IProperty
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * f: ('A -> 'B -> 'C -> 'D -> U2<bool, unit>) -> IProperty
        abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * genE: ValueGenerator<'E> * f: ('A -> 'B -> 'C -> 'D -> 'E -> U2<bool, unit>) -> IProperty

        abstract gen: IGen with get

    [<ImportAll("testcheck")>]
    let private tc: ITestCheck = jsNative

    let check (prop: IProperty) = tc.check(prop)
    let sampleOne (generator: ValueGenerator<_>) = tc.sampleOne(generator)
    let sampleOneWithSize generator size = tc.sampleOne(generator, size)
    let sample generator = tc.sample generator
    let sampleWithCount generator count = tc.sample(generator, count)

    type Property =
        [<ImportMember("testcheck")>]
        static member property (gen1: ValueGenerator<'T1>, propertyFn: 'T1 -> bool) : IProperty = jsNative
        [<ImportMember("testcheck")>]
        static member property (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, propertyFn: 'T1 -> 'T2 -> bool) : IProperty = jsNative

    [<RequireQualifiedAccess>]
    module Gen =
        let any = tc.gen.any
        let primitive = tc.gen.primitive
        let boolean = tc.gen.boolean
        let ``null`` = tc.gen.``null``
        let undefined = tc.gen.undefined
        let NaN = tc.gen.NaN
        let number = tc.gen.number
        let posNumber = tc.gen.posNumber
        let negNumber = tc.gen.negNumber
        let numberWithin min max = tc.gen.numberWithin(min, max)
        let int = tc.gen.int
        let posInt = tc.gen.posInt
        let negInt = tc.gen.negInt
        let sPosInt = tc.gen.sPosInt
        let sNegInt = tc.gen.sNegInt
        let intWithin min max = tc.gen.intWithin (min, max)
        let string = tc.gen.string
        let asciiString = tc.gen.asciiString
        let alphaNumString = tc.gen.alphaNumString
        let substring original = tc.gen.substring original
        let char = tc.gen.char
        let asciiChar = tc.gen.asciiChar
        let alphaNumChar = tc.gen.alphaNumChar
        
        let array generator = tc.gen.array generator
        let arrayWithSize generator size = tc.gen.array (generator, size)
        let object generator = tc.gen.object generator
        let objectWithSize generator (size: int) = tc.gen.object (generator, size)
        let objectKeyValue (keyGen: ValueGenerator<'Key>) (valueGen: ValueGenerator<'Value>) = tc.gen.object (keyGen, valueGen)
        let arrayOrObject generator = tc.gen.arrayOrObject generator
        let nested collectionGen valueGen = tc.gen.nested(collectionGen, valueGen)

        let retn value = tc.gen.``return``(value) 
        let deepCopyOf value = tc.gen.deepCopyOf value
        let sized genFn = tc.gen.sized genFn

    type Gen() =
        [<Emit("testcheck.gen.oneOf([$0, $1])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>) : ValueGenerator<U2<'T1, 'T2>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>) : ValueGenerator<U3<'T1, 'T2, 'T3>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2, $3])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>, gen4: ValueGenerator<'T4>) : ValueGenerator<U4<'T1, 'T2, 'T3, 'T4>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2, $3, $4])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>, gen4: ValueGenerator<'T4>, gen5: ValueGenerator<'T5>) : ValueGenerator<U5<'T1, 'T2, 'T3, 'T4, 'T5>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2, $3, $4, $5])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>, gen4: ValueGenerator<'T4>, gen5: ValueGenerator<'T5>, gen6: ValueGenerator<'T6>) : ValueGenerator<U6<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2, $3, $4, $5, $6])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>, gen4: ValueGenerator<'T4>, gen5: ValueGenerator<'T5>, gen6: ValueGenerator<'T6>, gen7: ValueGenerator<'T7>) : ValueGenerator<U7<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>> = jsNative
        [<Emit("testcheck.gen.oneOf([$0, $1, $2, $3, $4, $5, $6, $7])")>]
        static member oneOf (gen1: ValueGenerator<'T1>, gen2: ValueGenerator<'T2>, gen3: ValueGenerator<'T3>, gen4: ValueGenerator<'T4>, gen5: ValueGenerator<'T5>, gen6: ValueGenerator<'T6>, gen7: ValueGenerator<'T7>, gen8: ValueGenerator<'T8>) : ValueGenerator<U8<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'T8>> = jsNative

        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>) : ValueGenerator<U2<'T1, 'T2>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>) : ValueGenerator<U3<'T1, 'T2, 'T3>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5], [$6, $7]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>, weight4: int, gen4: ValueGenerator<'T4>) : ValueGenerator<U4<'T1, 'T2, 'T3, 'T4>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5], [$6, $7], [$8, $9]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>, weight4: int, gen4: ValueGenerator<'T4>, weight5: int, gen5: ValueGenerator<'T5>) : ValueGenerator<U5<'T1, 'T2, 'T3, 'T4, 'T5>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5], [$6, $7], [$8, $9], [$10, $11]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>, weight4: int, gen4: ValueGenerator<'T4>, weight5: int, gen5: ValueGenerator<'T5>, weight6: int, gen6: ValueGenerator<'T6>) : ValueGenerator<U6<'T1, 'T2, 'T3, 'T4, 'T5, 'T6>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5], [$6, $7], [$8, $9], [$10, $11], [$12, $13]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>, weight4: int, gen4: ValueGenerator<'T4>, weight5: int, gen5: ValueGenerator<'T5>, weight6: int, gen6: ValueGenerator<'T6>, weight7: int, gen7: ValueGenerator<'T7>) : ValueGenerator<U7<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7>> = jsNative
        [<Emit("testcheck.gen.oneOfWeighted([[$0, $1], [$2, $3], [$4, $5], [$6, $7], [$8, $9], [$10, $11], [$12, $13], [$14, $15]])")>]
        static member oneOfWeighted (weight1: int, gen1: ValueGenerator<'T1>, weight2: int, gen2: ValueGenerator<'T2>, weight3: int, gen3: ValueGenerator<'T3>, weight4: int, gen4: ValueGenerator<'T4>, weight5: int, gen5: ValueGenerator<'T5>, weight6: int, gen6: ValueGenerator<'T6>, weight7: int, gen7: ValueGenerator<'T7>, weight8: int, gen8: ValueGenerator<'T8>) : ValueGenerator<U8<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7, 'T8>> = jsNative

module TestCheckMocha =
    open TestCheck

    let testProperty name property =
        testCase name <| fun _ ->
            let result = check property
            Expect.isTrue result.result (sprintf "Seed: %d<br>Failed for: %s" result.seed (Json.stringify result.fail))

open TestCheck
open TestCheckMocha

let testcheckTests =
    testList "TestCheck tests" [ 
        testCase "sampleOne generates values smaller then given size" <| fun _ ->
            let values = Seq.init 1024 (fun _ -> sampleOne Gen.int)
            Expect.isTrue (Seq.max values <= 30) "Should actually be smaller then 30"

        testCase "sample generates int smaller then given size" <| fun _ ->
            let values = sampleWithCount Gen.int 200
            Expect.isTrue (Seq.max values <= 200) "Should actually be smaller then 200"

        testCase "gen.any: print generated values" <| fun _ ->
            let values = sample Gen.any
            // values
            // |> Seq.iter (Json.stringify >> printfn "%s")
            Expect.pass () "Just show the values"

        testCase "gen.null generates just null values" <| fun _ ->
            let values = sample Gen.``null``
            Expect.isTrue (values |> Seq.forall isNull) "All values should be null"

        testCase "gen.numberWithin generates numbers withing given range" <| fun _ ->
            let values = sample <| Gen.numberWithin -10.5 10.5
            Expect.isTrue (Seq.max values <= 10.5) "All values should be smaller or equal given max value"
            Expect.isTrue (Seq.min values >= -10.5) "All values should be greater or equal given min value"

        testCase "gen.intWithin generates ints withing given range" <| fun _ ->
            let values = sample <| Gen.intWithin -10 10
            Expect.isTrue (Seq.max values <= 10) "All values should be smaller or equal given max value"
            Expect.isTrue (Seq.min values >= -10) "All values should be greater or equal given min value"

        testCase "gen.string generates strings with length smaller then size" <| fun _ ->
            let values = List.init 1024 (fun _ -> sampleOne Gen.string)
            Expect.isTrue (List.max (List.map String.length values) <= 30) "All values should be smaller or equal given max value"

        testCase "gen.char generates char or string?" <| fun _ ->
            Seq.init 1024 (fun _ -> sampleOne Gen.char)
            |> Seq.iter (fun c -> Expect.isTrue (c.GetType().Name = "Char") "Should be char")

        testCase "gen.array generates array without options" <| fun _ ->
            let someArray = sampleOne (Gen.array Gen.int)
            Expect.isTrue (someArray.Length <= 30) "Should be shorter or equal 30 elements"

        testCase "gen.array generates array with size options" <| fun _ ->
            let someArray = sampleOne (Gen.arrayWithSize Gen.int (size 20.0))
            Expect.isTrue (someArray.Length <= 1024) "Should be shorter or equal 1024 elements"

        testCase "gen.object" <| fun _ ->
            let someObj = sampleOne (Gen.object Gen.int)
            printfn "%s" (Json.stringify someObj)
            Expect.pass () "Should pass"

        testCase "gen.objectWithSize" <| fun _ ->
            let someObj = sampleOne (Gen.objectWithSize Gen.int 3)
            printfn "%s" (Json.stringify someObj)
            Expect.pass () "Should pass"

        testCase "gen.objectKeyValue" <| fun _ ->
            let value = sampleOne (Gen.objectKeyValue Gen.alphaNumString Gen.int)
            printfn "****************** gen.objectKeyValue BEGIN"
            printfn "%s" (Json.stringify value)
            printfn "****************** gen.objectKeyValue END"
            Expect.pass () "Should pass"

        testCase "gen.arrayOrObject" <| fun _ ->
            let values = sampleWithCount (Gen.arrayOrObject Gen.int) 2
            printfn "****************** gen.arrayOrObject BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.arrayOrObject END"
            Expect.pass () "Should pass"

        testCase "gen.nested" <| fun _ ->
            let values = sampleWithCount (Gen.array (Gen.array Gen.int)) 2
            printfn "****************** gen.nested BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.nested END"
            Expect.pass () "Should pass"

        testCase "gen.oneOf 2 generators" <| fun _ ->
            let values = sampleWithCount (Gen.oneOf (Gen.int, Gen.string)) 2
            printfn "****************** gen.oneOf 2 generators BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.oneOf 2 generators END"
            Expect.pass () "Should pass"

        testCase "gen.oneOf 3 generators" <| fun _ ->
            let values = sampleWithCount (Gen.oneOf (Gen.int, Gen.string, Gen.boolean)) 10
            printfn "****************** gen.oneOf 3 generators BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.oneOf 3 generators END"
            Expect.pass () "Should pass"

        testCase "gen.oneOf 8 generators" <| fun _ ->
            let values = sampleWithCount (Gen.oneOf (Gen.int, Gen.string, Gen.boolean, Gen.int, Gen.string, Gen.boolean, Gen.number, Gen.string)) 20
            printfn "****************** gen.oneOf 8 generators BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.oneOf 8 generators END"
            Expect.pass () "Should pass"

        testCase "gen.oneOfWeighted 8 generators" <| fun _ ->
            let values = sampleWithCount (Gen.oneOfWeighted (2, Gen.int, 8, Gen.string, 20, Gen.boolean)) 20
            printfn "****************** gen.oneOfWeighted 8 generators BEGIN"
            values |> Seq.iter (Json.stringify >> printfn "%s")
            printfn "****************** gen.oneOfWeighted 8 generators END"
            Expect.pass () "Should pass"

        testCase "gen.retn should always generate the same value" <| fun _ ->
            let values = sample <| Gen.retn 42
            Expect.isTrue (values |> Seq.forall ((=) 42)) "All values should be the meaning of life, universe and everything"

        testCase "gen.sized should pass the size to genFn" <| fun _ ->
            let gen = Gen.sized (fun size -> Gen.intWithin -size size)
            let values = sampleWithCount gen 30
            Expect.isTrue (values |> Seq.forall (fun x -> x >= -30 && x <= 30)) "All values should be between -size and size"

        testProperty "values is always smaller then 30" <| Property.property(Gen.int, fun value -> value <= 30)
        
        testProperty "first value is always smaller then the second one" <| Property.property(Gen.int, Gen.int, fun v1 v2 -> v1 <= v2)
    ]

let allTests = testList "All" [
    testcheckTests
]

[<EntryPoint>]
let main (args: string[]) = Mocha.runTests allTests