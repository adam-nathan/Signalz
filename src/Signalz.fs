namespace Signalz
open System.Collections.Generic

[<AutoOpen>]
module internal Utils = 
    let undefined<'a> : 'a = Unchecked.defaultof<'a>

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
open System
type Signal<'T when 'T : equality> (expr: unit -> 'T) as this = 
    let mutable value = undefined
    let mutable expr = expr
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

    do recompute()

    member __.Value =  
        ignore <| observers.Add caller.Value
        caller.Value.Observe this
        value

    member private __.Update (expr':unit -> 'T) = 
        expr <- expr'
        recompute ()

    static member (~~) (s:Signal<'T>) = s.Value
    static member (<~) (s:Signal<'T>, v) = s.Update (fun () -> v)

    interface Signal with 
        member __.ReCompute () = recompute ()
        member __.Observe s = ignore <| observed.Add s
        member __.Ignore s = ignore <| observed.Remove s
type 'a signal when 'a : equality = Signal<'a>  

[<RequireQualifiedAccess>]
module Signal = 
    let private (~~) (s:'a signal) : 'a = s.Value
    let private (<~) s v = Signal.(<~) (s,v)
    let constant x = Signal (fun _ -> x)
    let map f _1 = Signal (fun _ -> f ~~_1)
    let map2 f _1 _2 = Signal (fun _ -> f ~~_1 ~~_2)
    let map3 f _1 _2 _3 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3)
    let map4 f _1 _2 _3 _4 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4)
    let map5 f _1 _2 _3 _4 _5 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5)
    let map6 f _1 _2 _3 _4 _5 _6 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6)
    let map7 f _1 _2 _3 _4 _5 _6 _7 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6 ~~_7)
    let map8 f _1 _2 _3 _4 _5 _6 _7 _8 = Signal (fun _ -> f ~~_1 ~~_2 ~~_3 ~~_4 ~~_5 ~~_6 ~~_7 ~~_8)
    let foldp f acc x = let state = ref ~~acc in Signal (fun _ -> state := f !state ~~x; !state)
    let filter f x = foldp (fun prev next -> if f next then next else prev) (constant ~~x) x
    let merge x y = 
        let out = constant ~~x
        ignore <| Signal (fun _ -> out <~ ~~x) 
        ignore <| Signal (fun _ -> out <~ ~~y) 
        out
