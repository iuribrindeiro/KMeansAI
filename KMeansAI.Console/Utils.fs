module Utils

open System

let strJoin (by: string) (lst: 'a seq) = String.Join(by, lst)

module Seq =
    let shuffle lst =
        let random = System.Random()
        lst |> Seq.sortBy (fun _ -> random.Next())


    let takeRandom n lst = lst |> shuffle |> Seq.take n
