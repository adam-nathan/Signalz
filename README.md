# Signalz
A lightweight F# library for reactive programming using signals.

## Examples

```fsharp
open System
open System.Threading
open Signalz

let a = Signal (fun () -> 1)
let b = Signal (fun () -> a.Value + 1)
let c = Signal (fun () -> a.Value < b.Value)

let rand = Random()
for _ in {1..10} do
    a <~ rand.Next 10
    printfn "%d < %d is %b" a.Value b.Value c.Value
    Thread.Sleep 100
```