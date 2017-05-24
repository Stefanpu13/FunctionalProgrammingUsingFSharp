namespace Exercises2
open System
open System.Globalization
open System.IO
open System.Web

module E = 
    (* 10.2
        The HTML elements <pre> . . . </pre> encloses a pre-formatted part of the web-page. This
        part is displayed exactly as written, including spaces and line breaks, but each line should, of
        course, be encoded in HTML encoding. Parts of this text can be copied using copy-paste when
        the page is displayed using a web-browser. Make a program with program call
        examplePage fileName.txt
        that inputs the contents of the text file fileName.txt and produces a web-page fileName.html
        containing the contents of this file as preformatted text.
    *)
    let baseDir =  Directory.GetCurrentDirectory() + @"\10TextProcessing\10TextProcessing - Exercises - 2"
    let examplePage inputFile = 
        if File.Exists inputFile
        then
            let preamble =
                "<!DOCTYPE html>
                <html>
                    <head>
                        <title>F# Text processing - Exercise 2</title>
                    </head>
                    <body>
                        <h1>F# Text processing - Exercise 2</h1>"                        
                
            let postamble = "</body></html>"           
            let fileContent = File.ReadAllText inputFile
            let encodedContent ="<pre>" + (HttpUtility.HtmlEncode fileContent) + "</pre>"            
            let allHTMLContent = preamble + encodedContent + postamble
            
            File.WriteAllText ((baseDir + @"\files\fileName.html"), allHTMLContent)
        else
            failwith "file not found"

        
    examplePage (baseDir + @"\files\fileName.txt")

   