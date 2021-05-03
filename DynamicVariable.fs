namespace Signalz
open System.Threading

type DynamicVariable<'T>(init : 'T) = 
    let tl = 
        let ret = new ThreadLocal<'T>() in ret.Value <- init
        ret

    member __.Value = tl.Value
    member __.WithValue (newval:'T, thunk:unit -> 'S) : 'S = 
        let oldval = __.Value
        tl.Value <- newval
        try thunk ()
        finally tl.Value <- oldval

    override __.ToString() = $"Dynamic variable {tl.Value}"
