open Utils

type InputItem = {| x: float; y: float |}

type Centroid = { x: float; y: float }

type Item =
    { x: float
      y: float
      centroid: Centroid option
      centroidsDistances:
          {| centroid: Centroid
             distance: float |} list }

type KMeansExecution =
    { centroides: Centroid list
      items: Item list }

let createCentroidFromItem (inputItem: InputItem) = { x = inputItem.x; y = inputItem.y }

let toItemInCluster (inputItem: InputItem) =
    { x = inputItem.x
      y = inputItem.y
      centroid = None
      centroidsDistances = List.empty }

let createKMeansExecution items centroids =
    { centroides = centroids
      items = items |> List.map toItemInCluster }

let logExecution (msg: string) (kmeansExecution: KMeansExecution) =
    printfn "|%s|" msg |> ignore
    printfn "%A" kmeansExecution |> ignore
    kmeansExecution


// let choseRandomCentroides numberOfClusters items =
//     createKMeansExecution items [{ x = 3; y = 3; }; { x = 7; y = 5;}]

let choseRandomCentroides numberOfClusters items =
    items
    |> Seq.takeRandom numberOfClusters
    |> Seq.map createCentroidFromItem
    |> Seq.toList
    |> createKMeansExecution items

let calculateItemsDistanceFromCentroids (kmenasExecution: KMeansExecution) : KMeansExecution =
    let calculateDistanceFromCentroids (centroids: Centroid list) (item: Item) =
        let calculateDistanceFromCentroid (centroid: Centroid) =
            {| centroid = centroid
               distance = ((item.x - centroid.x) ** 2.0) + ((item.y - centroid.y) ** 2) |> sqrt |}

        { item with
            centroidsDistances = centroids 
            |> List.map calculateDistanceFromCentroid }

    { kmenasExecution with
        items =
            kmenasExecution.items 
            |> List.map (calculateDistanceFromCentroids kmenasExecution.centroides) }

let assosiateItemsToClosestCentroid (kmenasExecution) =
    let associateItemToClosestCentroid (item: Item) =
        { item with
            centroid =
                item.centroidsDistances
                |> Seq.minBy _.distance
                |> _.centroid
                |> Some }

    { kmenasExecution with
        items = kmenasExecution.items 
        |> List.map associateItemToClosestCentroid }

let repositionCentroids (kmeansExecution: KMeansExecution) =
    let repositionCentroid (centroid: Centroid) =
        let centroidItems =
            kmeansExecution.items
            |> List.filter (fun item -> item.centroid = Some centroid)

        let calculateAverage items property =
            items
            |> List.map property
            |> List.average

        { x = calculateAverage centroidItems _.x
          y = calculateAverage centroidItems _.y }

    { kmeansExecution with
        centroides = kmeansExecution.centroides |> List.map repositionCentroid }

let rec runKMeans kmeansExecution =
    let recalculateCentroidsOnChange newKmeansExecutions =
        if newKmeansExecutions.items = kmeansExecution.items then
            newKmeansExecutions
        else
            newKmeansExecutions 
            |> repositionCentroids 
            |> runKMeans

    kmeansExecution
    |> calculateItemsDistanceFromCentroids
    |> logExecution "Distances from centroids calculated"
    |> assosiateItemsToClosestCentroid
    |> logExecution "Associated items to closest centroid"
    |> recalculateCentroidsOnChange

[ {| x = 1.0; y = 2.0 |}
  {| x = 2.0; y = 2.0 |}
  {| x = 3.0; y = 3.0 |}
  {| x = 4.0; y = 1.0 |}
  {| x = 5.0; y = 2.0 |}
  {| x = 6.0; y = 3.0 |}
  {| x = 7.0; y = 5.0 |}
  {| x = 8.0; y = 9.0 |}
  {| x = 7.0; y = 7.0 |}
  {| x = 8.0; y = 8.0 |}
  {| x = 8.0; y = 6.0 |}
  {| x = 9.0; y = 10.0 |}
  {| x = 9.0; y = 11.0 |} ]
|> choseRandomCentroides 2
|> logExecution "Initial state"
|> runKMeans
|> logExecution "Final result" |> ignore
