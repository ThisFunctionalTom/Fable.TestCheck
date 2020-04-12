module Fable.TestCheck

open Fable.Core

type Error = System.Exception

type CheckOptions = {
    numTests: int
    maxSize: int
    seed: int
}

type CheckReturnShrunk<'TArgs> =
    abstract result: U2<bool, Error> with get, set
    abstract smallest: 'TArgs with get, set
    abstract depth: float with get, set
    abstract totalNodesVisited: float with get, set

type CheckReturn =
    abstract result: U2<bool, Error> with get, set
    abstract numTests: float with get, set
    abstract seed: float option with get, set
    abstract fail: 'TArgs option with get, set
    abstract failingSize: float option with get, set
    /// When a check fails, the failing arguments shrink to find the smallest
    /// value that fails.
    abstract shrunk: CheckReturnShrunk<'TArgs> option with get, set

type Property<'TArgs> =
    interface end

let testThis = "hello"

/// Given a property to check, return the result of the check.
/// 
/// If the property generates a false value, check will shrink the generator
/// and return a Result which includes the `shrunk` key.
/// 
/// If no options are provided, they default to:
/// 
///      {numTests: 100, maxSize: 200, seed: <Random>}
let [<Global>] check (property: Property<'TArgs>) (options: CheckOptions) : CheckReturn = jsNative

