namespace Signalz
open System.Collections.Generic

[<AutoOpen>]
module internal Utils = 
    let undefined<'a> : 'a = Unchecked.defaultof<'a>
    let safewrap e = fun () -> try Ok (e ()) with e -> Error e

module internal Internals = 
    type Signal = 
        abstract member ReCompute : unit -> unit
        abstract member Observe : Signal -> unit
        abstract member Ignore : Signal -> unit

    type NoSignal () =
        interface Signal with 
            member __.ReCompute () = ()
            member __.Observe _ = () 
            member __.Ignore _ = () 

    let caller = new DynamicVariable<Signal>(NoSignal())

open Internals
type Signal<'T when 'T : equality> (expr: unit -> 'T) as this = 
    let mutable expr = safewrap expr
    let mutable value = expr ()
    let observers = new HashSet<Signal>()
    let observed = new ResizeArray<Signal>()

    let recompute () = 
        for s in observed do s.Ignore this
        observed.Clear()

        let newValue = caller.WithValue(this, fun () -> expr ())
        if value <> newValue then
            value <- newValue

            let observers' = Seq.toList observers
            observers.Clear()
            for observer in observers' do 
                observer.ReCompute ()

    do recompute ()

    member __.Value =  
        ignore <| observers.Add caller.Value
        caller.Value.Observe this
        match value with Ok v -> v | Error e -> raise e
    member private __.Update (expr':unit -> 'T) = 
        expr <- safewrap expr'
        recompute ()

    static member (<~) (s:Signal<'T>, v:'T) = s.Update (fun () -> v)

    interface Signal with 
        member __.ReCompute () = recompute ()
        member __.Observe s = ignore <| observed.Add s
        member __.Ignore s = ignore <| observed.Remove s

type 'a signal when 'a : equality = Signal<'a>      // alias 