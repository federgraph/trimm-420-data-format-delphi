# Trimm420.app data format test code

This repo contains a Delphi FMX test project for **Trimm-Item** and **Trimm-File** instances.

Note that I added the `dproj` and the `res` file of the test project to `.gitignore`.
You need to recreate private versions of those (use new empty app).

In your dproj file, check that configuration of `output dir` and `output dir for units` in project options for Delphi-Compiler on all targets is set to the default of
```
.\$(Platform)\$(Config)
```

The code for this test project was extracted from the real application and shows one aspect only.

1. Start by having a look at the definition of TRggData in unit RiggVar.RG.Data.pas.
This is the class that holds the Trimm-Item data to be exchanged via clipboard or the file system.
1. Explore unit RiggVar.App.Main1.pas where instances of the class will be created.
1. Debug the application to see how a user provided piece of data is parsed.
1. Make sure that the text data you prepare for use with the application complies with the intention of the developer and just works.
1. Instead of guessing you can track down any misunderstanding.
1. Feel free to improve the UI of the test app.

The test project should explain how the real application will
- copy a Trimm-Item to the clipboard
- copy a Trimm-File to the clipboard
- past a Trimm-Item or Trimm-File from the clipboard
- write a Trimm-File to disk
- read a Trimm-File from disk at application startup time
- read a Trimm-File from disk at use request

To test drive the code I have created a minimal Form with normal Buttons and Memo Controls.
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

- The application can write to Trimm-File-Auto.txt
- The application will never write to Trimm-File.txt
- The application can read from Trimm-File.txt!

You should provide a Trimm-File.txt and press the `rtf` read button.
For Trimm-File.txt, start out with a copy of Trimm-File-Auto.txt,
which can be created with `wtf` button.

Monitor the clipboard content.
Put manually changed text into the clipboard and then paste into the application with `pti` button.
You can paste Trimm-Item text or Trimm-File text, the app will detect what it is and digest either.

## On Mac

It should work the same on a Mac.

The version of the real app that I have published uses **sandboxing**, it needs to and I want it.

But *sandboxing* is an overloaded term.
There is a global variable `IsSandboxed` in the application, located in RiggVar.App.Main.
If the log reports that IsSandboxed is true,
it means that the application will use a FileOpenDialog and FileSaveDialog whenever it want to access a file for reading or writing.

When IsSandboxed is true, Trimm-File-Auto.txt in the Documents folder is only a suggestion.
The user is in control and can choose a different name.

## On iPad

Attention: Boolean Variable `IsSandboxed` is False on iPad, even if the iPad version is of course sandboxed.

Sandboxing an app on iOS means that every app has its own Documents folder.

```
/var/mobile/Containers/Data/Application/.../Documents/
```

In terms of IsSandboxed it means that it should be false,
no dialog should be shown for the filename to be used,
the app will just read and write to the app specific Documents folder.

## On Android

I don't know.
Maybe later when the 64Bit compiler is available.
It will be similar to iOS, except that the location of the Documents folder is different.

## Next steps

The example test project (this one) should have a better user interface
while still focusing on the aspect of showing what is stored by the application and where.

Of course you could use it as a starting point for rewriting the app from scratch.

- You have the coordinates, draw a graph ...
- rotate, zoom, pan the graph
- allow the user to select the current parameter
- allow the user to change the current parameter value

The real application does all that, and it computes new coordinates for the model on the fly,
according to the latest change to the value of the current parameter.

If you, as a developer, come up with a new implementation of the application shell,
I may be able to help with the completion of the new app, with some code from the original.

Until then you could use the published app (see Store) and provide data in the format expected.