namespace Signalz
open System.Collections.Generic

[<AutoOpen>]
module internal Utils = 
    let undefined<'a> : 'a = Unchecked.defaultof<'a>
    let safewrap e = fun () -> try Ok (e ()) with ex -> Error ex

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
    let observers = new HashSet<Signal>()
    let observed = new ResizeArray<Signal>()

    let mutable value = caller.WithValue(this, fun () -> expr ())

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

    do 
        let observers' = Seq.toList observers
        observers.Clear()
        for observer in observers' do 
            observer.ReCompute ()

    member __.Value =  
        ignore <| observers.Add caller.Value
        caller.Value.Observe this
        match value with Ok v -> v | Error e -> raise e

    member private __.Update (expr':unit -> 'T) = 
        expr <- safewrap expr'
        recompute ()

    static member (~~) (s:Signal<_>) = s.Value
    static member (<~) (s:Signal<'T>, v:'T) = s.Update (fun () -> v)

    interface Signal with 
        member __.ReCompute () = recompute ()
        member __.Observe s = ignore <| observed.Add s
        member __.Ignore s = ignore <| observed.Remove s

type 'a signal when 'a : equality = Signal<'a>      // alias 

[<RequireQualifiedAccess>]
module Signal = 
    let private (~~) (s:'a signal) : 'a = s.Value
    let constant x = Signal (fun _ -> x)
    let map f _1 = Signal (fun _ -> f ~~_1)
    let map2 f _1 _2 = Signal (fun _ -> f ~~_1 ~~_2)
    let map3 f _1 _2 _3 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3)
    let map4 f _1 _2 _3 _4 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4)
    let map5 f _1 _2 _3 _4 _5 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5)
    let map6 f _1 _2 _3 _4 _5 _6 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6)
    let map7 f _1 _2 _3 _4 _5 _6 _7 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6 ~~_7)
    let map8 f _1 _2 _3 _4 _5 _6 _7 _8 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6 ~~_7 ~~_8)
    let filter predicate default' signal = Signal (fun _ -> if predicate ~~signal then ~~signal else default')
    let foldp folder init signal = 
        let state = ref init
        Signal (fun _ -> state := (folder ~~signal !state); !state)