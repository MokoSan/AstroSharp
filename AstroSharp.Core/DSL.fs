namespace AstroSharp.DSL

module PlanetarySystemDSL =

    open MathNet.Numerics

    type PlanetInfo = { Name : string; Radius : BigRational; Mass : BigRational } 

    type StarInfo = { Name : string; Radius : BigRational; Mass : BigRational }  
        with static member empty = { Name = ""; 
                                     Radius = BigRational.FromInt 0; 
                                     Mass = BigRational.FromInt 0; }

    type PlanetarySystemInfo = { Name : string; 
                                 Star : StarInfo; 
                                 Planets : PlanetInfo list }
        with static member empty = { Name = ""; Star = StarInfo.empty; Planets = [] }

module ParserUtils =

    open MathNet.Numerics

    let convertStringToBigRational ( input : string ) : BigRational = 
        if input.Contains("e") then 
            let splitInput = input.Split('e')
            let mantissa   = BigRational.FromInt( int splitInput.[0] ) 
            let exponent   = int splitInput.[1]
            mantissa * BigRational.Pow(10N, exponent) 
        else
            BigRational.Parse input

module Splitter =

    open System

    let splitters = [|' '; '\''; '\t'; '\n'; '\r' |]

    let split ( text : string ) : string list = 
        text.Split(splitters, StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList

module PlanetarySystemParser =

    open PlanetarySystemDSL 
    open ParserUtils

    let rec parsePlanetarySystem ( planetarySystem : PlanetarySystemInfo ) 
                                 ( listOfStrings : string list ) : PlanetarySystemInfo = 
        match listOfStrings with
        | "PlanetarySystem" :: systemName :: xs ->  
            let planetarySystem' = { planetarySystem with Name = systemName }
            parsePlanetarySystem planetarySystem' xs 
        | "Star" :: xs -> parseStarInfo planetarySystem xs
        | "Planet" :: xs -> parsePlanetInfo planetarySystem xs 
        | [] -> planetarySystem
        | x :: _ -> failwith "Error while parsing Planetary Info"

    and parseStarInfo ( planetarySystemInfo : PlanetarySystemInfo ) 
                      ( listOfStrings : string list ) : PlanetarySystemInfo = 
        match listOfStrings with
        | name :: mass :: radius :: xs ->
            let parsedMass   = convertStringToBigRational mass
            let parsedRadius = convertStringToBigRational radius
            let starInfo = { Name = name; 
                             Mass = parsedMass; 
                             Radius = parsedRadius }
            let planetarySystemInfo' = { planetarySystemInfo with Star = starInfo } 
            parsePlanetarySystem planetarySystemInfo' xs
        | _ -> failwith "Error while parsing Star Info" 

    and parsePlanetInfo ( planetarySystemInfo : PlanetarySystemInfo ) 
                        ( listOfStrings : string list ) : PlanetarySystemInfo = 
        match listOfStrings with
        | name :: radius :: mass :: xs -> 
            let parsedMass   = convertStringToBigRational mass
            let parsedRadius = convertStringToBigRational radius
            let planetInfo : PlanetInfo = { Name = name; 
                                            Mass = parsedMass; 
                                            Radius = parsedRadius } 
            let planetarySystemInfo' = 
              { planetarySystemInfo with 
                    Planets = planetInfo :: planetarySystemInfo.Planets } 
            parsePlanetarySystem planetarySystemInfo' xs
        | _ -> failwith "Error while parsing Planet Info"

    let createPlanetarySystem ( input : string ) : PlanetarySystemInfo = 
        input
        |> Splitter.split
        |> parsePlanetarySystem PlanetarySystemInfo.empty