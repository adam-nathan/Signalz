let todo<'a> : 'a = Unchecked.defaultof<'a>
let print x = printfn $"{x}"

open System
#r @"nuget: FsCheck"
#r @"nuget: Unquote"
#r @"../../bin/Debug/net5.0/Signalz.dll"
open Signalz
open FsCheck
open Swensen.Unquote

module Signal = 
    let constant x = Signal (fun () -> x)
    let map (f:'a -> 'b) (s:'a signal) : 'b signal = Signal (fun () -> f s.Value)

let inline (!) (x:'a) : 'a signal = Signal.constant x
let inline (!.) (f:Lazy<'a>) : 'a signal = Signal (fun () -> f.Value)
let inline (~~) (s:'a signal) = s.Value

let ``a signal of a ring is a ring`` a b c = 
    // setup
    let a,b,c = !a,!b,!c
    let inline (+) s t = !. (lazy (~~s + ~~t))
    let inline (*) s t = !. (lazy (~~s * ~~t))
    let inline minus s = Signal.map (fun x -> -x) s
    let inline (=!) x y = ~~x =! ~~y

    // check
    (((a + b) + c) =! (a + (b + c)))        
    ((a + b) =! (b + a))                    
    ((a + !0) =! a)                         
    ((a + minus a) =! !0)                     
    ((a * b) * c) =! (a * (b * c))          
    ((a * !1) =! a) ; ((!1 * a) =! a)
    (a * (b + c)) =! ((a * b) + (a * c))
    (b + c) * a =! (b * a) + (c * a)

Check.Quick ``a signal of a ring is a ring``

