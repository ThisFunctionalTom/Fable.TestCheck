// ts2fable 0.8.0
module rec Fable.TestCheckDt
open System
open Fable.Core
open Fable.Core.JS

type Array<'T> = System.Collections.Generic.IList<'T>
type Error = System.Exception

let [<Import("gen","testcheck")>] gen<'T, 'C> : Gen<'T, 'C> = jsNative

type [<AllowNullLiteral>] IExports =
    abstract ValueGenerator: ValueGeneratorStatic
    /// Given a property to check, return the result of the check.
    /// 
    /// If the property generates a false value, check will shrink the generator
    /// and return a Result which includes the `shrunk` key.
    /// 
    /// If no options are provided, they default to:
    /// 
    ///      {numTests: 100, maxSize: 200, seed: <Random>}
    abstract check: property: Property<'TArgs> * ?options: CheckOptions -> CheckReturn
    /// Creates a "property" as needed by `check`.
    /// 
    /// Accepts any number of value generators, the results of which become the
    /// arguments of the property function. The property function should return
    /// true if the property is upheld, or false if it fails.
    /// 
    ///      var numGoUp = property(gen.int, gen.posInt, (a, b) => a + b > a);
    ///      check(numGoUp, {times: 1000});
    abstract property: genA: ValueGenerator<'A> * f: ('A -> U2<bool, unit>) -> Property<'A>
    abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * f: ('A -> 'B -> U2<bool, unit>) -> Property<'A * 'B>
    abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * f: ('A -> 'B -> 'C -> U2<bool, unit>) -> Property<'A * 'B * 'C>
    abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * f: ('A -> 'B -> 'C -> 'D -> U2<bool, unit>) -> Property<'A * 'B * 'C * 'D>
    abstract property: genA: ValueGenerator<'A> * genB: ValueGenerator<'B> * genC: ValueGenerator<'C> * genD: ValueGenerator<'D> * genE: ValueGenerator<'E> * f: ('A -> 'B -> 'C -> 'D -> 'E -> U2<bool, unit>) -> Property<'A * 'B * 'C * 'D * 'E>
    /// Handy tool for checking the output of your generators. Given a generator,
    /// it returns an array of the results of the generator.
    /// 
    ///      var results = sample(gen.int);
    ///      // [ 0, 1, 1, 2, 3, 3, -6, 1, -3, -8 ]
    /// 
    /// By default 10 samples are provided unless otherwise specified.
    abstract sample: gen: ValueGenerator<'T> * ?numValues: float -> Array<'T>
    /// Handy tool for visualizing the output of your ValueGenerator.
    /// 
    /// Given a ValueGenerator, it returns a single value generated for a given `size`.
    /// 
    ///      sampleOne(gen.int)
    ///      // 24
    /// 
    /// By default, values of size 30 are produced.
    abstract sampleOne: gen: ValueGenerator<'T> * ?size: float -> 'T

type [<AllowNullLiteral>] CheckReturn =
    abstract result: U2<bool, Error> with get, set
    abstract numTests: float with get, set
    abstract seed: float option with get, set
    abstract fail: 'TArgs option with get, set
    abstract failingSize: float option with get, set
    /// When a check fails, the failing arguments shrink to find the smallest
    /// value that fails.
    abstract shrunk: CheckReturnShrunk<'TArgs> option with get, set

/// Options to be passed to array() or object()
type [<AllowNullLiteral>] SizeOptions =
    /// If provided, the exact size of the resulting collection.
    abstract size: float option with get, set
    /// If provided, the minimum size of the resulting collection.
    abstract minSize: float option with get, set
    /// If provided, the maximum size of the resulting collection.
    abstract maxSize: float option with get, set

/// ValueGenerators of values.
type [<AllowNullLiteral>] ValueGenerator<'T> =
    /// Creates a new ValueGenerator which also sometimes generates null values.
    abstract nullable: unit -> ValueGenerator<'T option>
    /// Creates a new ValueGenerator which generates non-empty values.
    /// 
    /// Examples of empty values are 0, "", null, [], and {}
    abstract notEmpty: unit -> ValueGenerator<'T>
    /// Creates a new ValueGenerator which ensures that all values generated adhere to
    /// the given predicate function.
    /// 
    /// For example, to create a ValueGenerator of any number except multiples of 5:
    /// 
    ///      var genAnythingBut5s = gen.int.suchThat(n => n % 5 !== 0);
    /// 
    /// Note: Care is needed to ensure there is a high chance the predicate will
    /// pass, after ten attempts, an exception will throw.
    abstract suchThat: fn: ('T -> bool) -> ValueGenerator<'T>
    /// Creates a new ValueGenerator that depends on the values of this ValueGenerator.
    /// 
    /// For example, to create a ValueGenerator of square numbers:
    /// 
    ///      var genSquares = gen.int.then(n => n * n);
    /// 
    /// For example, to create a ValueGenerator which first generates an array of
    /// integers, and then returns both that array and a sampled value from it:
    /// 
    ///      var genList = gen.notEmpty(gen.array(gen.int))
    ///      var genListAndItem = genList.then(
    ///        list => [ list, gen.oneOf(list) ]
    ///      );
    abstract ``then``: fn: ('T -> U2<ValueGenerator<'U>, 'U>) -> ValueGenerator<'U>
    /// Creates a new ValueGenerator which grows at a different scale.
    /// 
    /// ValueGenerators start by producing very "small" values (closer to 0) at first,
    /// and produce larger values in later iterations of a test as a result of a
    /// "size" value which grows with each generation. Typically "size" grows
    /// linearly, but .scale() can alter a size to grow at different rates.
    /// 
    /// For example, to generate "big" numbers that grow super-linearly (cubicly):
    /// 
    ///       var bigInts = gen.int.scale(n => n * n * n)
    ///       console.log(sample(bigInts))
    ///       // [ 0, 1, 5, 0, -59, -56, -160, 261, 409, -34 ]
    /// 
    /// Note: When shrinking a failing test, "size" gets smaller. If the scale
    /// function returns a value that's not dependent on it's input, then the
    /// resulting ValueGenerator will not shrink.
    abstract scale: fn: (float -> float) -> ValueGenerator<'T>
    /// Creates a new ValueGenerator which will never shrink.
    /// This is useful when shrinking is taking a long time or is not applicable.
    abstract neverShrink: unit -> ValueGenerator<'T>
    /// Creates a new ValueGenerator which will always consider shrinking, even if the
    /// property passes (up to one additional level).
    abstract alwaysShrink: unit -> ValueGenerator<'T>

/// ValueGenerators of values.
type [<AllowNullLiteral>] ValueGeneratorStatic =
    [<Emit "new $0($1...)">] abstract Create: unit -> ValueGenerator<'T>

/// Properties created by property()
type [<AllowNullLiteral>] Property<'TArgs> =
    interface end

/// The options accepted by check()
type [<AllowNullLiteral>] CheckOptions =
    abstract numTests: float option with get, set
    abstract maxSize: float option with get, set
    abstract seed: float option with get, set

type JSONPrimitive =
    U3<string, float, bool> option

type [<AllowNullLiteral>] JSONArray =
    inherit Array<JSONValue>

type JSONValue =
    JSONValue of U3<JSONPrimitive, JSONArray, JSONValue>

type [<AllowNullLiteral>] GenUniqueArray =
    [<Emit "$0($1...)">] abstract Invoke: valueGen: ValueGenerator<'T> * ?options: SizeOptions -> ValueGenerator<Array<'T>>
    [<Emit "$0($1...)">] abstract Invoke: valueGen: ValueGenerator<'T> * uniqueBy: ('T -> U2<string, float>) * ?options: SizeOptions -> ValueGenerator<Array<'T>>

type [<AllowNullLiteral>] GenObjectInvokeValueGenerator =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> 'T with get, set

type [<AllowNullLiteral>] GenObject =
    [<Emit "$0($1...)">] abstract Invoke: valueGen: ValueGenerator<'T> * ?options: SizeOptions -> ValueGenerator<GenObjectInvokeValueGenerator>
    [<Emit "$0($1...)">] abstract Invoke: keyGen: ValueGenerator<string> * valueGen: ValueGenerator<'T> * ?options: SizeOptions -> ValueGenerator<GenObjectInvokeValueGenerator>

type [<AllowNullLiteral>] GenArrayOrObjectValueGenerator<'T> =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> 'T with get, set
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: float -> 'T with get, set

type [<AllowNullLiteral>] GenJSONValueGenerator =
    [<Emit "$0[$1]{{=$2}}">] abstract Item: key: string -> JSONValue with get, set

type [<AllowNullLiteral>] Gen<'T, 'C> =
    /// Generates a specific shape of values given an initial nested Array or Object which
    /// contain *Generators*. Any values within the provided shape which don't contain
    /// generators will be *copied* (with `gen.deepCopy()`).
    /// 
    /// Note: Whenever a non-*Generator* is provided to a function which expects a *Generator*,
    /// it is converted to a *Generator* with `gen()`. That makes calling this function
    /// optional for most cases, unless trying to be explicit.
    /// 
    /// There are a few forms `gen()` can be used:
    /// 
    /// - Generate an Array shape with different values at each index (also known as "tuples")
    /// 
    ///    For example, a tuples of [ "constant", *int*, *bool* ] like `['foo', 3, true]`:
    /// 
    ///    ```js
    ///    gen([ 'foo', gen.int, gen.boolean ])
    ///    ```
    /// 
    /// - Generate an Object shape with different values for each key (also known as "records")
    /// 
    ///    For example, a record of { x: "constant", y: *int*, z: *bool* } like `{ x: 'foo', y: -4, z: false }`:
    /// 
    ///    ```js
    ///    gen({ x: 'foo', y: gen.int, z: gen.boolean })
    ///    ```
    /// 
    /// - Combinations of Array and Object shapes with generators at any point:
    /// 
    ///    For example, a data shape for a complex "place" data shape might look like:
    /// 
    ///    ```js
    ///    gen({
    ///      type: 'Place',
    ///      name: gen.string,
    ///      location: [ gen.number, gen.number ],
    ///      address: {
    ///        street: gen.string,
    ///        city: gen.string
    ///      }
    ///    })
    ///    ```
    [<Emit "$0($1...)">] abstract Invoke: tupleGens: U2<'T1, ValueGenerator<'T1>> * U2<'T2, ValueGenerator<'T2>> * U2<'T3, ValueGenerator<'T3>> * U2<'T4, ValueGenerator<'T4>> * U2<'T5, ValueGenerator<'T5>> -> ValueGenerator<'T1 * 'T2 * 'T3 * 'T4 * 'T5>
    [<Emit "$0($1...)">] abstract Invoke: tupleGens: U2<'T1, ValueGenerator<'T1>> * U2<'T2, ValueGenerator<'T2>> * U2<'T3, ValueGenerator<'T3>> * U2<'T4, ValueGenerator<'T4>> -> ValueGenerator<'T1 * 'T2 * 'T3 * 'T4>
    [<Emit "$0($1...)">] abstract Invoke: tupleGens: U2<'T1, ValueGenerator<'T1>> * U2<'T2, ValueGenerator<'T2>> * U2<'T3, ValueGenerator<'T3>> -> ValueGenerator<'T1 * 'T2 * 'T3>
    [<Emit "$0($1...)">] abstract Invoke: tupleGens: U2<'T1, ValueGenerator<'T1>> * U2<'T2, ValueGenerator<'T2>> -> ValueGenerator<'T1 * 'T2>
    [<Emit "$0($1...)">] abstract Invoke: tupleGens: U2<'T1, ValueGenerator<'T1>> -> ValueGenerator<'T1>
    [<Emit "$0($1...)">] abstract Invoke: genMap: obj -> ValueGenerator<'T>
    /// Generates any JS value, including Arrays and Objects (possibly nested).
    abstract any: ValueGenerator<obj option> with get, set
    /// Generates any primitive JS value:
    /// strings, numbers, booleans, null, undefined, or NaN.
    abstract primitive: ValueGenerator<U3<string, float, bool> option> with get, set
    abstract boolean: ValueGenerator<bool> with get, set
    abstract ``null``: ValueGenerator<obj> with get, set
    abstract undefined: ValueGenerator<obj> with get, set
    abstract NaN: ValueGenerator<float> with get, set
    /// Generates floating point numbers (including +Infinity, -Infinity, and NaN).
    abstract number: ValueGenerator<float> with get, set
    /// Generates only positive numbers (0 though +Infinity), does not generate NaN.
    abstract posNumber: ValueGenerator<float> with get, set
    /// Generates only negative numbers (0 though -Infinity), does not generate NaN.
    abstract negNumber: ValueGenerator<float> with get, set
    /// Generates a floating point number within the provided (inclusive) range.
    /// Does not generate NaN or +-Infinity.
    abstract numberWithin: (float -> float -> ValueGenerator<float>) with get, set
    /// ValueGenerator integers (32-bit signed) including negative numbers and 0.
    abstract int: ValueGenerator<float> with get, set
    /// Generates positive integers, including 0.
    abstract posInt: ValueGenerator<float> with get, set
    /// Generates negative integers, including 0.
    abstract negInt: ValueGenerator<float> with get, set
    /// Generates only strictly positive integers, not including 0.
    abstract sPosInt: ValueGenerator<float> with get, set
    /// Generates only strictly negative integers, not including 0.
    abstract sNegInt: ValueGenerator<float> with get, set
    /// Generates an integer within the provided (inclusive) range.
    /// The resulting ValueGenerator is not shrinkable.
    abstract intWithin: (float -> float -> ValueGenerator<float>) with get, set
    /// Generates strings of arbitrary characters.
    /// 
    /// Note: strings of arbitrary characters may result in higher-plane Unicode
    /// characters and non-printable characters.
    abstract string: ValueGenerator<string> with get, set
    /// Generates strings of printable ascii characters.
    abstract asciiString: ValueGenerator<string> with get, set
    /// Generates strings of only alpha-numeric characters: a-z, A-Z, 0-9.
    abstract alphaNumString: ValueGenerator<string> with get, set
    /// Generates substrings of an original string (including the empty string).
    abstract substring: (string -> ValueGenerator<string>) with get, set
    /// Generates arbitrary 1-byte characters (code 0 through 255).
    abstract char: ValueGenerator<string> with get, set
    /// Generates only printable ascii characters (code 32 through 126).
    abstract asciiChar: ValueGenerator<string> with get, set
    /// Generates only alpha-numeric characters: a-z, A-Z, 0-9.
    abstract alphaNumChar: ValueGenerator<string> with get, set
    /// Generates Arrays of values. There are a few forms `gen.array` can be used:
    /// 
    ///   - Generate Arrays of random sizes (ex. arrays of integers)
    /// 
    ///      gen.array(gen.int)
    /// 
    ///   - Generate Arrays of specific sizes (ex. length of 5)
    /// 
    ///      gen.array(gen.int, { size: 5 })
    /// 
    ///   - Generate Arrays of random sizes within a specific range
    ///     (ex. between 2 and 10)
    /// 
    ///      gen.array(gen.int, { minSize: 2, maxSize: 10 })
    abstract array: (ValueGenerator<'T> -> SizeOptions -> ValueGenerator<Array<'T>>) with get, set
    /// Generates Arrays of unique values.
    /// 
    /// Accepts the same size options as gen.array()
    /// 
    /// Optionally also accepts a function to determine how a value is unique
    abstract uniqueArray: GenUniqueArray with get, set
    /// Generates Objects of values. There are a few forms `gen.object` can be used:
    /// 
    ///   - Generate Objects with a specified kind of value and alpha-numeric keys.
    /// 
    ///      gen.object(gen.int)
    /// 
    ///   - Generate Objects with a specified kind of key and value,
    ///     (ex. numeric keys)
    /// 
    ///      gen.object(gen.int, gen.int)
    abstract ``object``: GenObject with get, set
    /// Generates either an Array or an Object with values of the provided kind.
    /// 
    /// Note: Objects will be produced with alpha-numeric keys.
    abstract arrayOrObject: (ValueGenerator<'T> -> ValueGenerator<GenArrayOrObjectValueGenerator<'T>>) with get, set
    /// Given a function which takes a generator and returns a generator (such as
    /// `gen.array` or `gen.object`), and a ValueGenerator to use as values, creates
    /// potentially nested values.
    /// 
    ///      gen.nested(gen.array, gen.int)
    ///      // [ [ 0, [ -2 ], 1, [] ]
    abstract nested: ((ValueGenerator<'T> -> ValueGenerator<'C>) -> ValueGenerator<'T> -> ValueGenerator<'C>) with get, set
    /// Generates JSON objects where each key is a JSON value.
    abstract JSON: ValueGenerator<GenJSONValueGenerator> with get, set
    /// Generates JSON values: primitives, or (possibly nested) arrays or objects.
    abstract JSONValue: ValueGenerator<JSONValue> with get, set
    /// Generates JSON primitives: strings, numbers, booleans and null.
    abstract JSONPrimitive: ValueGenerator<JSONPrimitive> with get, set
    /// Creates a ValueGenerator which will generate values from one of the
    /// provided generators.
    /// 
    ///      var numOrBool = gen.oneOf([gen.int, gen.boolean])
    abstract oneOf: (Array<U2<ValueGenerator<'T>, 'T>> -> ValueGenerator<'T>) with get, set
    /// Similar to `gen.oneOf()`, except provides probablistic "weights" to
    /// each generator.
    /// 
    ///      var numOrRarelyBool = gen.oneOfWeighted([[99, gen.int], [1, gen.boolean]])
    abstract oneOfWeighted: (Array<float * U2<ValueGenerator<'T>, 'T>> -> ValueGenerator<'T>) with get, set
    /// Creates a ValueGenerator which will always generate references of the provided value.
    /// 
    ///      var alwaysBlue = gen.return('blue');
    abstract ``return``: ('T -> ValueGenerator<'T>) with get, set
    /// Creates a ValueGenerator which will always generate deep copies of the provided value.
    /// 
    ///      var threeThings = gen.deepCopyOf([1,2,3]);
    abstract deepCopyOf: ('T -> ValueGenerator<'T>) with get, set
    /// Creates a ValueGenerator that relies on a size. Size allows for the "shrinking"
    /// of ValueGenerators. Larger "size" should result in a larger generated value.
    /// 
    /// For example, `gen.int` is shrinkable because it is implemented as:
    /// 
    ///      var gen.int = gen.sized(size => gen.intWithin(-size, size))
    abstract sized: ((float -> U2<ValueGenerator<'T>, 'T>) -> ValueGenerator<'T>) with get, set

type [<AllowNullLiteral>] CheckReturnShrunk<'TArgs> =
    abstract result: U2<bool, Error> with get, set
    abstract smallest: 'TArgs with get, set
    abstract depth: float with get, set
    abstract totalNodesVisited: float with get, set