## Test Project

RG14.dpr is the data format test project.

Note that I added the `dproj` and the `res` file of the test project to `.gitignore`.

> You need to recreate private versions of those (use an new empty app).

You could just remove the xml from RG14.dproj.xml, which is an empty one for 10.3.3 Rio, and let the IDE generate a new res file.

Please check that configuration of `output dir` and `output dir for units` in project options for Delphi-Compiler on all targets is set to the default of
```
.\$(Platform)\$(Config)
```

The code for this test project was extracted from the real application and shows one aspect only.

1. Start by having a look at the definition of TRggData in unit RiggVar.RG.Data.pas.
   This is the class that holds the Trimm-Item data to be exchanged via clipboard or the file system.
1. Explore unit RiggVar.App.Main1.pas where instances of the class will be created.
1. Step through the application code to see how a user provided piece of data is parsed.
1. Make sure that the text data you prepare for use with the application complies with the intention of the developer and just works.
1. Instead of guessing you can track down any misunderstanding.
1. Feel free to improve the UI of the test app.

The test project should explain how the real application will
- copy a Trimm-Item to the clipboard
- copy a Trimm-File to the clipboard
- paste a Trimm-Item or Trimm-File from the clipboard
- write a Trimm-File to disk
- read a Trimm-File from disk at application startup time
- read a Trimm-File from disk at user request

To test drive the code I have created a minimal Form with normal Buttons and Memo controls.
The UI for this initial version of the test app was done super quick, it is ugly by design,
but can be understood easily.
You click a button and follow the code.
In a next version I may improve on the UI, so that it is usable on a phone.

Note that I optimize for project comparability.
That is why I keep the layout and names of the original source files and use almost empty classes in some places.
The point is that I need to be able to compare projects, e.g. the test project against the real project.

You should be able to run the application on latest Windows, Mac, iPad, and perhaps even on Android.

## On Windows

Windows is great for debugging.

Watch these two files in your User\Documents folder:
```
Trimm-File.txt
Trimm-File-Auto.txt
```
Both text files should be UTF-8 encoded, with BOM.

- The application can write to Trimm-File-Auto
- The application can read from your Trimm-File (and from Trimm-File-Auto)
- The application will NOT auto-write to Trimm-File

You should provide a Trimm-File.txt in the expected location and press the `rtf` button to try and read it.
For Trimm-File.txt, start out with a copy of Trimm-File-Auto.txt,
which can be created with `wtf` button.

Monitor the clipboard. I use notepad.exe to manually put
content into the clipboard and then paste into the application with `pti` button.

> You can paste Trimm-Item text or Trimm-File text, the app will detect what it is and digest either.

## On Mac

It should work almost the same on a Mac.
I use the Notes Mac App to manage content.
- From a Note in Notes I copy content to the clipboard.
- In a Note (new Note is great) I retrieve (paste) content from the clipboard.

The version of the real app that I have published uses **sandboxing**, it needs to and I want it.

But *sandboxing* is an overloaded term.
There is a global variable `IsSandboxed` in the application, located in RiggVar.App.Main.
If the log reports that IsSandboxed is true,
it means that the application will use a FileOpenDialog and FileSaveDialog whenever it wants to access a file for reading or writing,
which is in harmony with the idea of sandboxing an app on the Mac platform.

When IsSandboxed is true, Trimm-File.txt in the Documents folder is only a suggestion.
The user is in control and can choose a different name.
But it should have a .txt extension, be plain UTF-8 encoded text, and preferably have a BOM.
If all this is true it should work.

## On iPad

The Trimm420 app is supposed to support exchanging text data via the **Notes** App.
This would be the *Notizen App* in German language.
A **Note** holding a Trimm-Item or a Trimm-File can have a life in the cloud and be the means of sharing data between Mac and iPad.

Once you have pasted data into the app you may want to use the `wtf` button in order to save it to Trimm-File-Auto.txt,
so that it will be loaded when the app is started up the next time.
It is your decision to update the file on disk, it is not done automatically.

Attention: Boolean Variable `IsSandboxed` is False on iPad, even if the iPad version is of course sandboxed.
Sandboxing an app on iOS means that every app has its own Documents folder.
```
/var/mobile/Containers/Data/Application/.../Documents/
```

So yes, variable IsSandboxed should be false,
no dialog window should be shown for the filename to be used,
the app will read and write to the app specific Documents folder,
using the automatically chosen filename,
according to the convention.

Note that you cannot create and maintain a Trimm-File.txt in the sandboxed Documents folder.
This is why the `rtf` button in the application will try and read Trimm-File-Auto.txt,
instead of Trimm-File.txt,
as it would on Windows or Mac.
This is a subtle platform diff of the concept.
Take a note!

## On Android

I don't know.
Maybe later when the 64Bit compiler is available.
It will be similar to iOS, except that the location of the Documents folder is different.
There is an old app in the Playstore, it needs to be updated.

## Next steps

The example test project (this one) should have a better user interface
while still focusing on the aspect of showing what is stored by the application and where.

Of course you could use it as a starting point for rewriting the app from scratch.

- You have the coordinates, draw a graph ...
- rotate, zoom, pan the graph
- allow the user to select the current parameter
- allow the user to change the current parameter value

The real application does it, and it updates computed values on the fly.

If you can come up with a new implementation of the application shell
I may be able to help with the completion of the app.
