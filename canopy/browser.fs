[<AutoOpen>]
module canopy.browser

open OpenQA.Selenium.Firefox
open OpenQA.Selenium
open OpenQA.Selenium.Support.UI
open OpenQA.Selenium.Interactions
open SizSelCsZzz
open Microsoft.FSharp.Core.Printf
open System.IO
open System
open configuration
open levenshtein
open reporters
open types
open finders

let sleep seconds =
    let ms = match box seconds with
              | :? int as i -> i * 1000
              | :? float as i -> Convert.ToInt32(i * 1000.0)
              | _ -> 1000
    System.Threading.Thread.Sleep(ms)

type CanopyBrowser(browser:IWebDriver) =
    
    member val wipTest = false with get, set
    member val searchedFor = [] with get, set

    member this.browser = browser

    member this.screenshot directory filename =
        let pic = (browser :?> ITakesScreenshot).GetScreenshot().AsByteArray
        if not <| Directory.Exists(directory) 
            then Directory.CreateDirectory(directory) |> ignore
        IO.File.WriteAllBytes(Path.Combine(directory,filename + ".png"), pic)
        pic
    
    member this.js script = (browser :?> IJavaScriptExecutor).ExecuteScript(script)

    member private this.swallowedJs script = try this.js script |> ignore with | ex -> ()

    member this.puts text = 
        reporter.write text
        let escapedText = System.Web.HttpUtility.JavaScriptStringEncode(text)
        let info = "
            var infoDiv = document.getElementById('canopy_info_div'); 
            if(!infoDiv) { infoDiv = document.createElement('div'); } 
            infoDiv.id = 'canopy_info_div'; 
            infoDiv.setAttribute('style','position: absolute; border: 1px solid black; bottom: 0px; right: 0px; margin: 3px; padding: 3px; background-color: white; z-index: 99999; font-size: 20px; font-family: monospace; font-weight: bold;'); 
            document.getElementsByTagName('body')[0].appendChild(infoDiv); 
            infoDiv.innerHTML = 'locating: " + escapedText + "';"
        this.swallowedJs info

    member this.describe = this.puts

    member private this.wait timeout f =
        let wait = new WebDriverWait(browser, TimeSpan.FromSeconds(timeout))
        wait.Until(fun _ -> (
                                try
                                    (f ()) = true
                                with
                                | :? CanopyException as ce -> raise(ce)
                                | _ -> false
                            )
                    ) |> ignore        
        ()

    member private this.colorizeAndSleep cssSelector =
        this.puts cssSelector
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #FFF467';" cssSelector
        sleep wipSleep    
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #ACD372';" cssSelector

    member this.highlight cssSelector = 
        this.swallowedJs <| sprintf "document.querySelector('%s').style.border = 'thick solid #ACD372';" cssSelector

    member this.suggestOtherSelectors cssSelector =     
        if not disableSuggestOtherSelectors then
            let allElements = browser.FindElements(By.CssSelector("html *")) |> Array.ofSeq
            let classesViaJs = """
                var classes = [];
                var all = document.getElementsByTagName('*');
                for (var i=0, max=all.length; i < max; i++) {
	                var ary = all[i].className.split(' ');
	                for(var j in ary){
		                if(ary[j] === ''){
			                ary.splice(j,1);
			                j--;
		                }
	                }
                   classes = classes.concat(ary);
                }
                return classes;"""
            let idsViaJs = """
                var ids = [];
                var all = document.getElementsByTagName('*');
                for (var i=0, max=all.length; i < max; i++) {
	                if(all[i].id !== "") {
		                ids.push(all[i].id);
	                }   
                }
                return ids;"""
            let valuesViaJs = """
                var values = [];
                var all = document.getElementsByTagName('*');
                for (var i=0, max=all.length; i < max; i++) {
	                if(all[i].value && all[i].value !== "") {
		                values.push(all[i].value);
	                }   
                }
                return values;"""
            let textsViaJs = """
                var texts = [];
                var all = document.getElementsByTagName('*');
                for (var i=0, max=all.length; i < max; i++) {
	                if(all[i].text && all[i].tagName !== 'SCRIPT' && all[i].text !== "") {
		                texts.push(all[i].text);
	                }   
                }
                return texts;"""
            let classes = this.js classesViaJs :?> System.Collections.ObjectModel.ReadOnlyCollection<System.Object> |> Seq.map (fun item -> "." + item.ToString()) |> Array.ofSeq
            let ids = this.js idsViaJs :?> System.Collections.ObjectModel.ReadOnlyCollection<System.Object> |> Seq.map (fun item -> "#" + item.ToString()) |> Array.ofSeq
            let values = this.js valuesViaJs :?> System.Collections.ObjectModel.ReadOnlyCollection<System.Object> |> Seq.map (fun item -> item.ToString()) |> Array.ofSeq
            let texts = this.js textsViaJs :?> System.Collections.ObjectModel.ReadOnlyCollection<System.Object> |> Seq.map (fun item -> item.ToString()) |> Array.ofSeq
            Array.append classes ids
            |> Array.append values
            |> Array.append texts
            |> Seq.distinct |> List.ofSeq 
            |> remove "." |> remove "#" |> Array.ofList
            |> Array.Parallel.map (fun u -> levenshtein cssSelector u)
            |> Array.sortBy (fun r -> r.distance)
            |> Seq.take 5
            |> Seq.map (fun r -> r.selector) |> List.ofSeq
            |> (fun suggestions -> reporter.suggestSelectors cssSelector suggestions)

    member this.waitFor (f : unit -> bool) =
        try        
            this.wait compareTimeout f
        with
            | :? WebDriverTimeoutException -> 
                    this.puts "Condition not met in given amount of time. If you want to increase the time, put compareTimeout <- 10.0 anywhere before a test to increase the timeout"
                    raise (CanopyWaitForException(sprintf "waitFor condition failed to become true in %.1f seconds" compareTimeout))

    //find related    
    member private this.findElements cssSelector (searchContext : ISearchContext) : IWebElement list =
        this.searchedFor <- (cssSelector, browser.Url) :: this.searchedFor
        let findInIFrame () =
            let iframes = findByCss "iframe" searchContext.FindElements
            if iframes.IsEmpty then 
                browser.SwitchTo().DefaultContent() |> ignore
                []
            else
                let webElements = ref []
                iframes |> List.iter (fun frame -> 
                    browser.SwitchTo().Frame(frame) |> ignore
                    webElements := this.findElements cssSelector searchContext
                )
                !webElements

        try
            let results =
                configuredFinders cssSelector searchContext.FindElements        
                |> Seq.filter(fun list -> not(list.IsEmpty))
            if Seq.isEmpty results then
                findInIFrame()
            else
               results |> Seq.head
        with | ex -> []

    member private this.findByFunction cssSelector timeout waitFunc searchContext reliable =
        if this.wipTest then this.colorizeAndSleep cssSelector
        let wait = new WebDriverWait(browser, TimeSpan.FromSeconds(elementTimeout))
        try
            if reliable then
                let elements = ref []
                wait.Until(fun _ -> 
                    elements := waitFunc cssSelector searchContext
                    not <| List.isEmpty !elements) |> ignore
                !elements
            else
                wait.Until(fun _ -> waitFunc cssSelector searchContext)
        with
            | :? WebDriverTimeoutException ->   
                this.suggestOtherSelectors cssSelector
                raise (CanopyElementNotFoundException(sprintf "can't find element %s" cssSelector))

    member private this.find cssSelector timeout searchContext reliable =
        (this.findByFunction cssSelector timeout this.findElements searchContext reliable).Head

    member private this.findMany cssSelector timeout searchContext reliable =
        this.findByFunction cssSelector timeout this.findElements searchContext reliable

    //get elements
    member private this.elementFromList cssSelector elementsList =
        match elementsList with
        | [] -> null
        | x :: [] -> x    
        | x :: y :: _ -> 
            if throwIfMoreThanOneElement then raise (CanopyMoreThanOneElementFoundException(sprintf "More than one element was selected when only one was expected for selector: %s" cssSelector))
            else x

    member private this.someElementFromList cssSelector elementsList =
        match elementsList with
        | [] -> None    
        | x :: [] -> Some(x)
        | x :: y :: _ -> 
            if throwIfMoreThanOneElement then raise (CanopyMoreThanOneElementFoundException(sprintf "More than one element was selected when only one was expected for selector: %s" cssSelector))
            else Some(x)

    member this.elements cssSelector = this.findMany cssSelector elementTimeout browser true

    member this.unreliableElements cssSelector = this.findMany cssSelector elementTimeout browser false
    
    member this.unreliableElement cssSelector = cssSelector |> this.unreliableElements |> this.elementFromList cssSelector

    member this.element cssSelector = cssSelector |> this.elements |> this.elementFromList cssSelector

    member this.elementWithin cssSelector (elem:IWebElement) =  this.find cssSelector elementTimeout elem true

    member this.parent elem = elem |> this.elementWithin ".."

    member this.elementsWithin cssSelector elem = this.findMany cssSelector elementTimeout elem true

    member this.unreliableElementsWithin cssSelector elem = this.findMany cssSelector elementTimeout elem false

    member this.someElement cssSelector = cssSelector |> this.unreliableElements |> this.someElementFromList cssSelector

    member this.someElementWithin cssSelector elem = elem |> this.unreliableElementsWithin cssSelector |> this.someElementFromList cssSelector

    member this.someParent elem = elem |> this.elementsWithin ".." |> this.someElementFromList "provided element"

    member this.nth index cssSelector = List.nth (this.elements cssSelector) index

    member this.first cssSelector = (this.elements cssSelector).Head

    member this.last cssSelector = (List.rev (this.elements cssSelector)).Head

//read/write
    member private this.writeToSelect cssSelector text =
        let elem = this.element cssSelector
        let options = Seq.toList (elem.FindElements(By.XPath(sprintf "option[text()='%s']" text)))
        match options with
        | [] -> raise (CanopyOptionNotFoundException(sprintf "element %s does not contain value %s" cssSelector text))
        | head::tail -> head.Click()

    member this.( << ) cssSelector text = 
        this.wait elementTimeout (fun _ ->        
            let writeToElement (e : IWebElement) =
                if e.TagName = "select" then
                    this.writeToSelect cssSelector text
                else
                    let readonly = e.GetAttribute("readonly")
                    if readonly = "true" then
                        raise (CanopyReadOnlyException(sprintf "element %s is marked as read only, you can not write to read only elements" cssSelector))
                    try e.Clear() with ex -> ex |> ignore
                    e.SendKeys(text)

            let atleastOneItemWritten = ref false
            this.elements cssSelector
            |> List.iter (fun elem -> 
                try  
                    writeToElement elem
                    atleastOneItemWritten := true
                with
                    | :? CanopyReadOnlyException as ex -> raise ex
                    | _ -> ())
            !atleastOneItemWritten)

    member private this.textOf (element : IWebElement) =
        match element.TagName  with
        | "input" ->
            element.GetAttribute("value")
        | "textarea" ->
            element.GetAttribute("value")
        | "select" ->
            let value = element.GetAttribute("value")
            let options = Seq.toList (element.FindElements(By.TagName("option")))
            let option = options |> List.filter (fun e -> e.GetAttribute("value") = value)
            option.Head.Text
        | _ ->
            element.Text    

    member this.read item = 
        match box item with
        | :? IWebElement as elem -> this.textOf elem
        | :? string as cssSelector -> this.element cssSelector |> this.textOf
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't read %O because it is not a string or element" item))
        
    member this.clear item =
        let clear cssSelector (elem : IWebElement) =
            let readonly = elem.GetAttribute("readonly")
            if readonly = "true" then raise (CanopyReadOnlyException(sprintf "element %s is marked as read only, you can not clear read only elements" cssSelector))
            elem.Clear()
    
        match box item with
        | :? IWebElement as elem -> clear elem.TagName elem
        | :? string as cssSelector -> this.element cssSelector |> clear cssSelector 
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't clear %O because it is not a string or element" item))

    //status
    member this.selected item = 
        let selected cssSelector (elem : IWebElement) =
            if not <| elem.Selected then raise (CanopySelectionFailedExeception(sprintf "element selected failed, %s not selected." cssSelector))

        match box item with
        | :? IWebElement as elem -> selected elem.TagName elem
        | :? string as cssSelector -> this.element cssSelector |> selected cssSelector
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't check selected on %O because it is not a string or element" item))
    
    member this.deselected item =     
        let deselected cssSelector (elem : IWebElement) =
            if elem.Selected then raise (CanopyDeselectionFailedException(sprintf "element deselected failed, %s selected." cssSelector))

        match box item with
        | :? IWebElement as elem -> deselected elem.TagName elem
        | :? string as cssSelector -> this.element cssSelector |> deselected cssSelector
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't check deselected on %O because it is not a string or element" item))

    member this.press key = 
        let elem = ((this.js "return document.activeElement;") :?> IWebElement)
        elem.SendKeys(key)

    //alerts
    member this.alert() = 
        this.waitFor (fun _ ->
            browser.SwitchTo().Alert() |> ignore
            true)
        browser.SwitchTo().Alert()

    member this.acceptAlert() = 
        this.wait compareTimeout (fun _ ->
            browser.SwitchTo().Alert().Accept()
            true)

    member this.dismissAlert() = 
        this.wait compareTimeout (fun _ ->
            browser.SwitchTo().Alert().Dismiss()
            true)

    //assertions    
    member this.( == ) item value =
        match box item with
        | :? IAlert as alert -> 
            let text = alert.Text
            if text <> value then   
                alert.Dismiss()
                raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %s, got: %s" value text))
        | :? string as cssSelector -> 
            let bestvalue = ref ""
            try
                this.wait compareTimeout (fun _ -> ( let readvalue = (this.read cssSelector)
                                                if readvalue <> value && readvalue <> "" then
                                                    bestvalue := readvalue
                                                    false
                                                else
                                                    readvalue = value))
            with
                | :? CanopyElementNotFoundException as ex -> raise (CanopyEqualityFailedException(sprintf "%s\r\nequality check failed.  expected: %s, got: %s" ex.Message value !bestvalue))
                | :? WebDriverTimeoutException -> raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %s, got: %s" value !bestvalue))

        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't check equality on %O because it is not a string or alert" item))

    member this.( != ) cssSelector value =
        try
            this.wait compareTimeout (fun _ -> (this.read cssSelector) <> value)
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyNotEqualsFailedException(sprintf "%s\r\nnot equals check failed.  expected NOT: %s, got: " ex.Message value))
            | :? WebDriverTimeoutException -> raise (CanopyNotEqualsFailedException(sprintf "not equals check failed.  expected NOT: %s, got: %s" value (this.read cssSelector)))
        
    member this.( *= ) cssSelector value =
        try        
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> (this.textOf element) = value)))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyValueNotInListException(sprintf "%s\r\ncan't find %s in list %s\r\ngot: " ex.Message value cssSelector))
            | :? WebDriverTimeoutException -> 
                let sb = new System.Text.StringBuilder()
                cssSelector |> this.elements |> List.iter (fun e -> bprintf sb "%s\r\n" (this.textOf e))
                raise (CanopyValueNotInListException(sprintf "can't find %s in list %s\r\ngot: %s" value cssSelector (sb.ToString())))

    member this.( *!= ) cssSelector value =
        try
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> (this.textOf element) = value) |> not))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyValueInListException(sprintf "%s\r\nfound check failed" ex.Message))
            | :? WebDriverTimeoutException -> raise (CanopyValueInListException(sprintf "found %s in list %s, expected not to" value cssSelector))
    
    member this.contains (value1 : string) (value2 : string) =
        if (value2.Contains(value1) <> true) then
            raise (CanopyContainsFailedException(sprintf "contains check failed.  %s does not contain %s" value2 value1))

    member this.count cssSelector count =
        try        
            this.wait compareTimeout (fun _ -> (this.unreliableElements cssSelector).Length = count)
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyCountException(sprintf "%s\r\ncount failed. expected: %i got: %i" ex.Message count 0))
            | :? WebDriverTimeoutException -> raise (CanopyCountException(sprintf "count failed. expected: %i got: %i" count (this.unreliableElements cssSelector).Length))

    member private this.regexMatch pattern input = System.Text.RegularExpressions.Regex.Match(input, pattern).Success

    member this.elementsWithText cssSelector regex =
        this.unreliableElements cssSelector
        |> List.filter (fun elem -> this.regexMatch regex (this.textOf elem))

    member this.elementWithText cssSelector regex = (this.elementsWithText cssSelector regex).Head

    member this.( =~ ) cssSelector pattern =
        try
            this.wait compareTimeout (fun _ -> this.regexMatch pattern (this.read cssSelector))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyEqualityFailedException(sprintf "%s\r\nregex equality check failed.  expected: %s, got:" ex.Message pattern))
            | :? WebDriverTimeoutException -> raise (CanopyEqualityFailedException(sprintf "regex equality check failed.  expected: %s, got: %s" pattern (this.read cssSelector)))

    member this.( *~ ) cssSelector pattern =
        try        
            this.wait compareTimeout (fun _ -> ( cssSelector |> this.elements |> Seq.exists(fun element -> this.regexMatch pattern (this.textOf element))))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyValueNotInListException(sprintf "%s\r\ncan't regex find %s in list %s\r\ngot: " ex.Message pattern cssSelector))
            | :? WebDriverTimeoutException -> 
                let sb = new System.Text.StringBuilder()
                cssSelector |> this.elements |> List.iter (fun e -> bprintf sb "%s\r\n" (this.textOf e))
                raise (CanopyValueNotInListException(sprintf "can't regex find %s in list %s\r\ngot: %s" pattern cssSelector (sb.ToString())))

    member this.is expected actual =
        if expected <> actual then
            raise (CanopyEqualityFailedException(sprintf "equality check failed.  expected: %O, got: %O" expected actual))

    member this.(===) expected actual = this.is expected actual

    member private this.shown (elem : IWebElement) =    
        let opacity = elem.GetCssValue("opacity")
        let display = elem.GetCssValue("display")
        display <> "none" && opacity = "1"
       
    member this.displayed item =
        try
            this.wait compareTimeout (fun _ -> 
                match box item with
                | :? IWebElement as element ->  this.shown element
                | :? string as cssSelector -> this.element cssSelector |> this.shown
                | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item)))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyDisplayedFailedException(sprintf "%s\r\ndisplay check for %O failed." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyDisplayedFailedException(sprintf "display check for %O failed." item))

    member this.notDisplayed item =
        try
            this.wait compareTimeout (fun _ -> 
                match box item with
                | :? IWebElement as element -> not(this.shown element)
                | :? string as cssSelector -> (this.unreliableElements cssSelector |> List.isEmpty) || not(this.unreliableElement cssSelector |> this.shown)
                | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item)))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyNotDisplayedFailedException(sprintf "%s\r\nnotDisplay check for %O failed." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyNotDisplayedFailedException(sprintf "notDisplay check for %O failed." item))

    member this.enabled item = 
        try
            this.wait compareTimeout (fun _ -> 
                match box item with
                | :? IWebElement as element -> element.Enabled = true
                | :? string as cssSelector -> (this.element cssSelector).Enabled = true
                | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item)))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyEnabledFailedException(sprintf "%s\r\nenabled check for %O failed." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyEnabledFailedException(sprintf "enabled check for %O failed." item))

    member this.disabled item = 
        try
            this.wait compareTimeout (fun _ -> 
                match box item with
                | :? IWebElement as element -> element.Enabled = false
                | :? string as cssSelector -> (this.element cssSelector).Enabled = false
                | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item)))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyDisabledFailedException(sprintf "%s\r\ndisabled check for %O failed." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyDisabledFailedException(sprintf "disabled check for %O failed." item))

    member this.fadedIn cssSelector = fun _ -> this.element cssSelector |> this.shown

    //clicking/checking
    member this.click item =     
        match box item with
        | :? IWebElement as element -> element.Click()
        | :? string as cssSelector ->
            this.wait elementTimeout (fun _ -> 
                let atleastOneItemClicked = ref false
                this.elements cssSelector
                |> List.iter (fun elem ->                                 
                    try
                        elem.Click()
                        atleastOneItemClicked := true
                    with | ex -> ())
                !atleastOneItemClicked)
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't click %O because it is not a string or webelement" item))
    
    member this.doubleClick item =
        let js = "var evt = document.createEvent('MouseEvents'); evt.initMouseEvent('dblclick',true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0,null); arguments[0].dispatchEvent(evt);"

        match box item with
        | :? IWebElement as elem -> (browser :?> IJavaScriptExecutor).ExecuteScript(js, elem) |> ignore
        | :? string as cssSelector ->         
            this.wait elementTimeout (fun _ -> 
                (let elem = this.element cssSelector
                (browser :?> IJavaScriptExecutor).ExecuteScript(js, elem) |> ignore; true))
        | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't doubleClick %O because it is not a string or webelement" item))

    member this.check item = 
        try
            match box item with
            | :? IWebElement as elem -> if not <| elem.Selected then this.click elem
            | :? string as cssSelector -> 
                this.waitFor (fun _ -> 
                    if not <| (this.element cssSelector).Selected then this.click cssSelector
                    (this.element cssSelector).Selected)        
            | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't read %O because it is not a string or element" item))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyCheckFailedException(sprintf "%s\r\nfailed to check %O." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyCheckFailedException(sprintf "failed to check %O." item))

    member this.uncheck item = 
        try
            match box item with
            | :? IWebElement as elem -> if elem.Selected then this.click elem
            | :? string as cssSelector -> 
                this.waitFor (fun _ -> 
                    if (this.element cssSelector).Selected then this.click cssSelector
                    (this.element cssSelector).Selected = false)        
            | _ -> raise (CanopyNotStringOrElementException(sprintf "Can't read %O because it is not a string or element" item))
        with
            | :? CanopyElementNotFoundException as ex -> raise (CanopyUncheckFailedException(sprintf "%s\r\nfailed to uncheck %O." ex.Message item))
            | :? WebDriverTimeoutException -> raise (CanopyUncheckFailedException(sprintf "failed to uncheck %O." item))

    //draggin
    member this.(-->) cssSelectorA cssSelectorB =
        this.wait elementTimeout (fun _ ->
            let a = this.element cssSelectorA
            let b = this.element cssSelectorB
            (new Actions(browser)).DragAndDrop(a, b).Perform()
            true)

    member this.drag cssSelectorA cssSelectorB = cssSelectorA this.(-->) cssSelectorB

    //browser related
    member this.pin direction =   
        let h = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height
        let w = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width
        let maxWidth = w / 2    
        browser.Manage().Window.Size <- new System.Drawing.Size(maxWidth,h)        
        match direction with
        | Left -> browser.Manage().Window.Position <- new System.Drawing.Point((maxWidth * 0),0)
        | Right -> browser.Manage().Window.Position <- new System.Drawing.Point((maxWidth * 1),0)
        | FullScreen -> browser.Manage().Window.Maximize()

    member this.currentUrl() = browser.Url

    member this.on (u: string) =
        let urlPath (u : string) =
            let url = match u with
                      | x when x.StartsWith("http") -> u  //leave absolute urls alone 
                      | _ -> "http://host/" + u.Trim('/') //ensure valid uri
            let uriBuilder = new System.UriBuilder(url)
            uriBuilder.Path.TrimEnd('/') //get the path part removing trailing slashes
        try
            this.wait pageTimeout (fun _ -> if browser.Url = u then true else urlPath(browser.Url) = urlPath(u))
        with
            | ex -> if browser.Url.Contains(u) = false then raise (CanopyOnException(sprintf "on check failed, expected expression '%s' got %s" u browser.Url))

    member this.( !^ ) (u : string) = browser.Navigate().GoToUrl(u)

    member this.url = this.(!^)

    member this.title() = browser.Title

    member this.reload = this.currentUrl >> this.url

    member this.coverage (url : 'a) =    
        let mutable innerUrl = ""
        match box url with    
        | :? string as u -> innerUrl <- u
        | _ -> innerUrl <- this.currentUrl()
        let nonMutableInnerUrl = innerUrl

        let selectors = 
            this.searchedFor 
            |> List.filter(fun (c, u) -> u = nonMutableInnerUrl) 
            |> List.map(fun (cssSelector, u) -> cssSelector) 
            |> Seq.distinct 
            |> List.ofSeq
    
        let script cssSelector = 
            "var results = document.querySelectorAll('" + cssSelector + "'); \
            for (var i=0; i < results.length; i++){ \
                results[i].style.border = 'thick solid #ACD372'; \
            }"
    
        //kinda silly but the app I am current working on will redirect you to login if you try to access a url directly, so dont try if one isnt passed in
        match box url with    
        | :? string as u -> this.url nonMutableInnerUrl
        |_ -> ()

        this.on nonMutableInnerUrl
        selectors |> List.iter(fun cssSelector -> this.swallowedJs (script cssSelector))
        let p = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), @"canopy\")
        let f = DateTime.Now.ToString("MMM-d_HH-mm-ss-fff")
        let ss = this.screenshot p f
        reporter.coverage nonMutableInnerUrl ss

    member this.innerSize() =
        let innerWidth = System.Int32.Parse((this.js "return window.innerWidth").ToString())
        let innerHeight = System.Int32.Parse((this.js "return window.innerHeight").ToString())
        innerWidth, innerHeight

    member this.resize size =
        let width,height = size
        let innerWidth, innerHeight = this.innerSize()
        let newWidth = browser.Manage().Window.Size.Width - innerWidth + width
        let newHeight = browser.Manage().Window.Size.Height - innerHeight + height
        browser.Manage().Window.Size <- System.Drawing.Size(newWidth, newHeight)   

    member this.rotate() =
        let innerWidth, innerHeight = this.innerSize()
        this.resize(innerHeight, innerWidth)

    member this.quit() =
        browser.Quit()