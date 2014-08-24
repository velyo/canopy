module canopy.finders

open OpenQA.Selenium
open SizSelCsZzz

//have to use the ReadonlyCollection<IWebElement> because thats what selenium uses and it wont cast to seq<IWebElement> or something, and type inference isnt playing nice
//basically a hack because I dont know a better way
let findByCss (cssSelector : string) (f : (By -> System.Collections.ObjectModel.ReadOnlyCollection<IWebElement>)) =
    try
        f(By.CssSelector(cssSelector)) |> List.ofSeq
    with | ex -> []
    
let findByJQuery cssSelector f =
    try
        f(ByJQuery.CssSelector(cssSelector)) |> List.ofSeq
    with | ex -> []

let findByXpath xpath f =
    try
        f(By.XPath(xpath)) |> List.ofSeq
    with | ex -> []

let findByLabel locator f =
    let isInputField (element : IWebElement) =
        element.TagName = "input" && element.GetAttribute("type") <> "hidden"
    
    let isField (element : IWebElement) =
        element.TagName = "select" || element.TagName = "textarea" || isInputField element

    let firstFollowingField (label : IWebElement) =
        let followingElements = label.FindElements(By.XPath("./following-sibling::*[1]")) |> Seq.toList
        match followingElements with
            | head :: tail when isField head-> [head]
            | _ -> []
    try
        let labels = f(By.XPath(sprintf ".//label[text() = '%s']" locator))
        if (Seq.isEmpty labels) then
            []
        else
            let (label : IWebElement) = (labels |> List.ofSeq).Head
            match label.GetAttribute("for") with
            | null -> firstFollowingField (labels |> List.ofSeq).Head
            | id -> f(By.Id(id)) |> List.ofSeq
    with | _ -> []

let findByText text f =
    try
        f(By.XPath(sprintf ".//*[text() = '%s']" text)) |> List.ofSeq
    with | _ -> []

let findByValue value f =
    try
        findByCss (sprintf "*[value='%s']" value) f |> List.ofSeq        
    with | _ -> []

let mutable htmlElements = [ "html"; "body"; "ul"; "li"; "a "; "p "; "div"; "span"; "table"; "tr"; "td"; "thead"; "tbody"; "label"; "input"; "button"; "select"; "option"; "textarea" ] //using a(space) instead of a because I dont want to match any word starting with a

let mutable selectorIsProbablyXpath = fun (selector : string) -> selector.StartsWith("/") || selector.Contains("id(") || selector.Contains("text()")
let mutable selectorIsProbablyJQuery = fun (selector : string) -> not <| selectorIsProbablyXpath selector && selector.Contains(":") //:eq :contains etc
let mutable selectorIsProbablyCSS = fun (selector : string) -> not <| selectorIsProbablyJQuery selector && (selector.StartsWith("#") || selector.StartsWith(".") || htmlElements |> List.exists (fun html -> selector.StartsWith(html)))
let mutable selectorIsProbablyValue = fun (selector : string) -> not <| selectorIsProbablyCSS selector
let mutable selectorIsProbablyLabel = fun (selector : string) -> not <| selectorIsProbablyCSS selector
let mutable selectorIsProbablyText = fun (selector : string) -> not <| selectorIsProbablyCSS selector

//you can use this as an example to how to extend canopy by creating your own set of finders, tweaking the current collection, or adding/removing
let mutable defaultFinders = 
    (fun cssSelector optimize f ->
        seq {
            if optimize then
                if selectorIsProbablyCSS cssSelector then yield findByCss cssSelector f
                if selectorIsProbablyValue cssSelector then yield findByValue cssSelector f
                if selectorIsProbablyXpath cssSelector then yield findByXpath cssSelector f
                if selectorIsProbablyLabel cssSelector then yield findByLabel cssSelector f
                if selectorIsProbablyText cssSelector then yield findByText cssSelector f
                if selectorIsProbablyJQuery cssSelector then yield findByJQuery cssSelector f
            else
                yield findByCss     cssSelector f
                yield findByValue   cssSelector f
                yield findByXpath   cssSelector f
                yield findByLabel   cssSelector f
                yield findByText    cssSelector f
                yield findByJQuery  cssSelector f
        }
    )