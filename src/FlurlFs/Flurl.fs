namespace Flurl
    

module QueryParam =

    open System
    open System.Linq
    open Microsoft.FSharp.Linq

    type queryValues =
    | String of string
    | StringArray of string []

    type Pair = string * queryValues


    /// <summary>
    /// Decodes a URL-encoded query string value.
    /// </summary>
    /// <param name="value">The encoded query string value.</param>
    /// <returns></returns>
    let decodeQueryParamValue (value:string) =
        // Uri.UnescapeDataString comes closest to doing it right, but famously stumbles on the + sign
        // http://weblog.west-wind.com/posts/2009/Feb/05/Html-and-Uri-String-Encoding-without-SystemWeb
        Uri.UnescapeDataString(value.Replace("+", " "))

    /// <summary>
    /// Parses a query string from a URL to a QueryParamCollection dictionary.
    /// </summary>
    /// <param name="queryString">The query string to parse.</param>
    /// <returns></returns>
    let parse (queryString:string) = 
        let qString = queryString.TrimStart([|'?'|]).Split([|'?'|]) |> Array.last
        let pairs = 
            qString.Split('&')
            |> Seq.ofArray
            |> Seq.map (fun kv -> kv.Split('=') |> fun arr -> arr.[0], decodeQueryParamValue arr.[1] |> String )
            |> Map.ofSeq 
        pairs    
            
[<AutoOpen>]
module FSharp =

    open System

    type Url = {
        Path:string
        //QueryParams: 
    }

    let private addTrailingSlash (path:string) = 
        if path.EndsWith ("/") then 
            path
        else
            path + "/"

    /// <summary>
    /// Encodes characters that are illegal in a URL path, including '?'. Does not encode reserved characters, i.e. '/', '+', etc.
    /// </summary>
    /// <param name="segment"></param>
    /// <returns></returns>
    let private cleanSegment (segment:string) =
        // http://stackoverflow.com/questions/4669692/valid-characters-for-directory-part-of-a-url-for-short-links
        let  unescaped = Uri.UnescapeDataString(segment)
        Uri.EscapeUriString(unescaped).Replace("?", "%3F")


    /// <summary>
    /// Appends a segment to the URL path, ensuring there is one and only one '/' character as a seperator.
    /// </summary>
    /// <param name="segment">The segment to append</param>
    /// <param name="encode">If true, URL-encode the segment where necessary</param>
    /// <returns>the Url object with the segment appended</returns>
    let appendPathSegment path (segment:string) =
        (path |> addTrailingSlash) + (cleanSegment (segment.TrimStart('/')))
