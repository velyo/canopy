[<AutoOpen>]
module canopy.core

open OpenQA.Selenium.Firefox
open OpenQA.Selenium
open OpenQA.Selenium.Support.UI
open OpenQA.Selenium.Interactions
open SizSelCsZzz
open Microsoft.FSharp.Core.Printf
open System.IO
open System
open browser
open configuration
open levenshtein
open reporters
open types
open finders

let mutable canopyBrowser = CanopyBrowser(null);
let mutable (browser : IWebDriver) = null;
let mutable (failureMessage : string) = null
let mutable wipTest = false
let searchedFor() = 
    canopyBrowser.searchedFor

let firefox = Firefox
let ie = IE
let chrome = Chrome
let phantomJS = PhantomJS
let phantomJSProxyNone = PhantomJSProxyNone
  
let mutable browsers = []

//misc
let addFinder finder =
    let currentFinders = configuredFinders
    configuredFinders <- (fun cssSelector f ->
        currentFinders cssSelector f
        |> Seq.append (seq { yield finder cssSelector f }))

let failsWith message = failureMessage <- message

let screenshot directory filename =
    canopyBrowser.screenshot directory filename
    
let js script = 
    canopyBrowser.js script

let sleep = canopy.browser.sleep    

let puts text = canopyBrowser.puts text

let describe = puts

let highlight cssSelector = canopyBrowser.highlight cssSelector

let suggestOtherSelectors cssSelector = canopyBrowser.suggestOtherSelectors cssSelector

let waitFor f = canopyBrowser.waitFor f

//get elements
let elements cssSelector = canopyBrowser.elements cssSelector

let unreliableElements cssSelector = canopyBrowser.unreliableElements cssSelector

let unreliableElement cssSelector = canopyBrowser.unreliableElement cssSelector

let element cssSelector = canopyBrowser.element cssSelector

let elementWithin cssSelector elem =  canopyBrowser.elementWithin cssSelector elem

let parent elem = canopyBrowser.parent elem

let elementsWithin cssSelector elem = canopyBrowser.elementsWithin cssSelector elem

let unreliableElementsWithin cssSelector elem = canopyBrowser.unreliableElementsWithin cssSelector elem

let someElement cssSelector = canopyBrowser.someElement cssSelector

let someElementWithin cssSelector elem = canopyBrowser.someElementWithin cssSelector elem

let someParent elem = canopyBrowser.someParent elem

let nth index cssSelector = canopyBrowser.nth index cssSelector

let first cssSelector = canopyBrowser.first cssSelector

let last cssSelector = canopyBrowser.last cssSelector

//read/write
let (<<) cssSelector text = canopyBrowser.(<<) cssSelector text

let read cssSelector = canopyBrowser.read cssSelector

let clear cssSelector = canopyBrowser.clear cssSelector

//status
let selected cssSelector = canopyBrowser.selected cssSelector

let deselected cssSelector = canopyBrowser.deselected cssSelector

//keyboard
let tab = Keys.Tab
let enter = Keys.Enter
let down = Keys.Down
let up = Keys.Up
let left = Keys.Left
let right = Keys.Right

let press key = canopyBrowser.press key

//alerts
let alert() = canopyBrowser.alert()

let acceptAlert() = canopyBrowser.acceptAlert()

let dismissAlert() = canopyBrowser.dismissAlert()

//assertions    
let ( == ) item value = canopyBrowser.( == ) item value

let ( != ) cssSelector value = canopyBrowser.( != ) cssSelector value
        
let ( *= ) cssSelector value = canopyBrowser.( *= ) cssSelector value

let ( *!= ) cssSelector value = canopyBrowser.( *!= ) cssSelector value

let contains value1 value2 = canopyBrowser.contains value1 value2

let count cssSelector count = canopyBrowser.count cssSelector count

let elementsWithText cssSelector regex = canopyBrowser.elementsWithText cssSelector regex

let elementWithText cssSelector regex = canopyBrowser.elementWithText cssSelector regex

let ( =~ ) cssSelector pattern = canopyBrowser.( =~ ) cssSelector pattern

let ( *~ ) cssSelector pattern = canopyBrowser.( *~ ) cssSelector pattern

let is expected actual = canopyBrowser.is expected actual

let (===) expected actual = canopyBrowser.(===) expected actual

let displayed cssSelector = canopyBrowser.displayed cssSelector

let notDisplayed cssSelector = canopyBrowser.notDisplayed cssSelector

let fadedIn cssSelector = canopyBrowser.fadedIn cssSelector

let enabled item = canopyBrowser.enabled item

let disabled item = canopyBrowser.disabled item

//clicking/checking
let click item = canopyBrowser.click item

let doubleClick item = canopyBrowser.doubleClick item

let check cssSelector = canopyBrowser.check cssSelector

let uncheck cssSelector = canopyBrowser.uncheck cssSelector

//draggin
let (-->) cssSelectorA cssSelectorB = canopyBrowser.(-->) cssSelectorA cssSelectorB

let drag cssSelectorA cssSelectorB = canopyBrowser.drag cssSelectorA cssSelectorB

//browser related
let pin direction = canopyBrowser.pin direction

let private chromeWithUserAgent userAgent =
    let options = Chrome.ChromeOptions()
    options.AddArgument("--user-agent=" + userAgent)
    new OpenQA.Selenium.Chrome.ChromeDriver(chromeDir, options) :> OpenQA.Selenium.IWebDriver

let private firefoxWithUserAgent (userAgent : string) = 
    let profile = FirefoxProfile()
    profile.SetPreference("general.useragent.override", userAgent)
    new OpenQA.Selenium.Firefox.FirefoxDriver(profile) :> OpenQA.Selenium.IWebDriver

let startBrowser b =
    //for chrome you need to download chromedriver.exe from http://code.google.com/p/chromedriver/wiki/GettingStarted
    //place chromedriver.exe in c:\ or you can place it in a customer location and change chromeDir value above
    //for ie you need to set Settings -> Advance -> Security Section -> Check-Allow active content to run files on My Computer*
    //also download IEDriverServer and place in c:\ or configure with ieDir
    //firefox just works
    //for phantomjs download it and put in c:\ or configure with phantomJSDir
       
    let browser =
        match b with
        | IE -> 
            new OpenQA.Selenium.IE.InternetExplorerDriver(ieDir) :> IWebDriver
        | IEWithOptions options ->
            new OpenQA.Selenium.IE.InternetExplorerDriver(ieDir, options) :> IWebDriver
        | IEWithOptionsAndTimeSpan(options, timeSpan) ->
            new OpenQA.Selenium.IE.InternetExplorerDriver(ieDir, options, timeSpan) :> IWebDriver
        | Chrome -> 
            new OpenQA.Selenium.Chrome.ChromeDriver(chromeDir) :> IWebDriver
        | ChromeWithOptions options ->
            new OpenQA.Selenium.Chrome.ChromeDriver(chromeDir, options) :> IWebDriver
        | ChromeWithUserAgent userAgent -> chromeWithUserAgent userAgent
        | ChromeWithOptionsAndTimeSpan(options, timeSpan) ->
            new OpenQA.Selenium.Chrome.ChromeDriver(chromeDir, options, timeSpan) :> IWebDriver
        | Firefox -> 
            new OpenQA.Selenium.Firefox.FirefoxDriver() :> IWebDriver
        | FirefoxWithProfile profile -> 
            new OpenQA.Selenium.Firefox.FirefoxDriver(profile) :> IWebDriver
        | FirefoxWithUserAgent userAgent -> firefoxWithUserAgent userAgent
        | PhantomJS -> 
            autoPinBrowserRightOnLaunch <- false
            new OpenQA.Selenium.PhantomJS.PhantomJSDriver(phantomJSDir) :> IWebDriver
        | PhantomJSProxyNone -> 
            autoPinBrowserRightOnLaunch <- false
            let service = OpenQA.Selenium.PhantomJS.PhantomJSDriverService.CreateDefaultService(canopy.configuration.phantomJSDir)
            service.ProxyType <- "none"
            new OpenQA.Selenium.PhantomJS.PhantomJSDriver(service) :> IWebDriver     
    CanopyBrowser(browser)

let start b =    
    let cb = startBrowser b
    canopyBrowser <- cb
    if autoPinBrowserRightOnLaunch = true then cb.pin Right
    browser <- cb.browser
    browsers <- browsers @ [browser]

let switchTo browserObj = 
    match box browserObj with
    | :? OpenQA.Selenium.IWebDriver as b -> 
        browser <- b
        canopyBrowser <- CanopyBrowser(b)
    | :? CanopyBrowser as cb -> 
        browser <- cb.browser
        canopyBrowser <- cb
    | _ -> failwithf "Cannot switch to browser '%O'" browserObj

let tile (browsers : OpenQA.Selenium.IWebDriver list) =   
    let h = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height
    let w = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width
    let count = browsers.Length
    let maxWidth = w / count

    let rec setSize (browsers : OpenQA.Selenium.IWebDriver list) c =
        match browsers with
        | [] -> ()
        | b :: tail -> 
            b.Manage().Window.Size <- new System.Drawing.Size(maxWidth,h)        
            b.Manage().Window.Position <- new System.Drawing.Point((maxWidth * c),0)
            setSize tail (c + 1)
    
    setSize browsers 0

let quit browser =
    reporter.quit()
    match box browser with
    | :? OpenQA.Selenium.IWebDriver as b -> b.Quit()
    | :? CanopyBrowser as cb -> cb.quit()
    | _ -> browsers |> List.iter (fun b -> b.Quit())

let currentUrl() = canopyBrowser.currentUrl()

let on u = canopyBrowser.on u

let ( !^ ) u = canopyBrowser.( !^ ) u

let url u = canopyBrowser.url u

let title() = canopyBrowser.title()

let reload() = canopyBrowser.reload()

let coverage url = canopyBrowser.coverage url

let innerSize() = canopyBrowser.innerSize()

let resize size = canopyBrowser.resize size

let rotate() = canopyBrowser.rotate()
