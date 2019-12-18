# Trimm420.app data format test code

This repo contains a Delphi FMX test project for **Trimm-Item** and **Trimm-File** instances.

## Trimm-Item example

This is what a Trimm-Item looks like when you copy to the clipboard and paste into notepad:
```
DOCTYPE := Trimm-Item;
Namespace := http://www.riggvar.de/2015/rgg/trimm-item ;
Version := 1;

Name := ;

A0X := 2560;
A0Y := 765;
A0Z := 430;
C0X := 4140;
C0Y := 0;
C0Z := 340;
D0X := 2870;
D0Y := 0;
D0Z := -100;
E0X := 2970;
E0Y := 0;
E0Z := 450;
F0X := -30;
F0Y := 0;
F0Z := 300;
MU := 2600;
MO := 2000;
ML := 6115;
MV := 5010;
CA := 50;
h0 := 56;
h2 := 0;
l2 := 100;
CPMin := 50;
CPPos := 100;
CPMax := 200;
SHMin := 140;
SHPos := 220;
SHMax := 300;
SAMin := 780;
SAPos := 850;
SAMax := 1000;
SLMin := 450;
SLPos := 479;
SLMax := 600;
SWMin := 0;
SWPos := 27;
SWMax := 89;
VOMin := 4400;
VOPos := 4500;
VOMax := 4600;
WIMin := 85;
WIPos := 94;
WIMax := 105;
WLMin := 4050;
WLPos := 4120;
WLMax := 4200;
WOMin := 2000;
WOPos := 2020;
WOMax := 2070;
```

## Trimm-File example

This is what a Trimm-File looks like when you copy to the clipboard and paste into notepad:
```
DOCTYPE := Trimm-File;
Namespace := http://www.riggvar.de/2015/rgg/trimm-file ;
Version := 1;

//Basis-Trimm (Trimm 0)

A0X := 2560;
A0Y := 765;
A0Z := 430;
C0X := 4140;
C0Y := 0;
C0Z := 340;
D0X := 2870;
D0Y := 0;
D0Z := -100;
E0X := 2970;
E0Y := 0;
E0Z := 450;
F0X := -30;
F0Y := 0;
F0Z := 300;

MU := 2600;
MO := 2000;
ML := 6115;
MV := 5000;
CA := 50;

h0 := 56;
h2 := 0;
l2 := 100;

CPMin := 50;
CPPos := 100;
CPMax := 200;

SHMin := 140;
SHPos := 220;
SHMax := 300;

SAMin := 780;
SAPos := 850;
SAMax := 1000;

SLMin := 450;
SLPos := 479;
SLMax := 600;

SWMin := 0;
SWPos := 27;
SWMax := 89;

VOMin := 4400;
VOPos := 4500;
VOMax := 4600;

WIMin := 85;
WIPos := 95;
WIMax := 105;

WLMin := 4050;
WLPos := 4120;
WLMax := 4200;

WOMin := 2000;
WOPos := 2020;
WOMax := 2070;

//Trimm1
Name := T1;
MV := 5010;

//Trimm2
Name := T2;
WLPos := 4140;

//Trimm3
Name := T3;
VOPos := 4530;

//Trimm4
Name := T4;
SHPos := 260;

//Trimm5
Name := T5;
SAPos := 900;

//Trimm6
Name := T6;
VOPos := 4560;
WLPos := 4100;
```

Up to 7 Trimm-Items will be evaluated when the application reads a Trimm-File,
- an unnamed Trimm-Item T0
- up to 6 named Trimm-Items T1-T6

Note that it is not necessary to repeat unchanged data.
Omitted data will be taken from built-in default Trimm,
which you will get when you call Reset on a TRggData instance.

Since the example data was generated by the app it does not use relative data.
When you edit a Trimm-File manually with an external editor or in any way you like,
you can chose to specify relative values.

## A side note

The six named Trimm-Items T1-T6 will be available for user-selection.

T0 is for comparison only, cannot be selected.
But the UI of the real app can show the diff between current and T0!
And T0 can be read from the Trimm-File, as the first Trimm-Item.

In the real app the user can move data from T1-T6 to current,
and from current to TO.

It is also possible to move data from TX to TY, when you use the clipboard
and change currently selected slot between copy and paste operations.

## Relative Values

It should be convenient for the users to specify the Trimm data for their boats.

Ok, I admit that it is currently *convenient* only if you know how.

When I say convenient, I mean two things:

- don't have to repeat unchanged values
- and (!) can specify relative values

```
//Trimm3
Name := T3;
//VOPos := 4530;
vo := 30; // 4500 + 30 = 4530
```

> Use small caps and no Pos for relative values.

In the snippet above, which is a possible replacement for Trimm3 in the Trimm-File example above,
I have used a relative value for VOPos, the current length of Vorstag (Forestay).

You can see this in the code below.

```pascal
{ from unit RiggVar.FB.DefConst }
const
  cCP = 'cp';
  cSH = 'sh';
  cSA = 'sa';
//  cSL = 'sl';
//  cSW = 'sw';
  cVO = 'vo';
  cWI = 'wi';
  cWL = 'wl';
  cWO = 'wo';

{ from unit RiggVar.RG.Data }
TRggData.Load(AML: TStrings)
var
  s: string;
begin
  //...
  s := AML.Values[cVO];
  VOPos := VOPos + StrToIntDef(s, 0);
  //...
end;
```

Relative means relative to the data already in the instance of the class.

Note that it is a method of the class that is called with a StringList of data.
You need to understand what instance is used to load the data,
which will become clear when you look into TRggData.ReadTrimmFile,
where Reset is called on the instance of TRggData.

> The default data is used as base for relative values.

At least this is the status quo right now.
Does it make sense?
As I am writing this, I am merely describing what has been implemented.
You can always file an issue, we are on GitHub.

By the way T7 (420) is the default data.

> Make T0 contain the same data as T7 (default) to see what you are diffing against in the Trimm-File. 

Someone has to start writing unit tests, perhaps when we do it in SwiftUI, or Flutter.
I am not going to change much, unless the unit tests are in place.
Unit testing should have its own heading.

## Unit testing

To be done. And should have its own readme file?

## TRggData instances

Just briefly, from the code, to make it clear:

```pascal
  TMain1 = class(TMain0)
  public
    RggData: TRggData;
    RggMain: TRggMain;

    Trimm0: TRggData;
    Trimm1: TRggData;
    Trimm2: TRggData;
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;
    Trimm7: TRggData; //420
    Trimm8: TRggData; //Logo    
end;
```

- I wanted to allow the user of the app to deal with a limited number of Trimms.
- TRggData is holding a Trimm-Item in Memory.
- T1-T6 are the user specified Trimms.
- T0 should hold a copy of T1-T6
- Diffing is an important feature. You are diffing against T0.
- T7 and T8 are read only.
- T7 is the default in Trimm420 app
- T7 would be a little different in Trimm470 app
- RggData is is temp object

The data structure the real application uses is part of class TRigg and is omitted from this test project.

## Trimm-Item Json

This is what a Trimm-Item *should* look like in json format:
```json
{"Name":"T0","Faktor":1,"OffsetX":0,"OffsetZ":0,
"RK":{
"A0":{"x":2560,"y":765,"z":430},
"C0":{"x":4140,"y":0,"z":340},
"D0":{"x":2870,"y":0,"z":-100},
"E0":{"x":2970,"y":0,"z":450},
"F0":{"x":-30,"y":0,"z":300}},
"RL":{"MU":2600,"MO":2000,"ML":6115,"MV":5000,"CA":50},
"SB":{
"CP":{"Min":50,"Pos":100,"Max":200},
"VO":{"Min":4400,"Pos":4500,"Max":4600},
"WI":{"Min":85,"Pos":95,"Max":105},
"WL":{"Min":4050,"Pos":4120,"Max":4200},
"WO":{"Min":2000,"Pos":2020,"Max":2070},
"SH":{"Min":140,"Pos":220,"Max":300},
"SA":{"Min":780,"Pos":850,"Max":1000},
"SL":{"Min":450,"Pos":479,"Max":600}}}
```

Not sure if I should format this properly or leave it as is.

```
RK = Rumpf-Koordinaten
RL = think Rigg-Längen or Mast-Längen ( ML already used for overall Mast-Length ) 
SB = Scroll-Bars
```

I happend to use scroll bars in 1995. It was the best thing available. We got new Trackbar controls later.
Today we have sliders. But all these things can be used to change the value of the current parameter.

The old scroll bar control used Integer values. The track bar and slider use single precision floating point values.
When you set up a modern slider you should perhaps allow input values from -1000 to +1000
and then map the value to the actual range of the currently selected parameter.
Take that as a suggestion from the original dev.
It has the advantage that it will be easier if you use only one or even none of these controls to change the value of the current parameter.

Sometimes it is enough to swipe across the screen in a certain way,
you do not need a control any more these days.
But I think one control, perhaps a slider, would be great.

## Trimm-Item Report

This is about the same data but formatted for display on screen.
```
A0 (2560, 765, 430)
C0 (4140, 0, 340)
D0 (2870, 0, -100)
E0 (2970, 0, 450)
F0 (-30, 0, 300)

ML, MV (6115, 5000)
MU, MO (2600, 2000)
CA (50)
h0, h2, l2 (56, 0, 100)

CP (50, 100, 200)
VO (4400, 4500, 4600)
WI (85, 95, 105)
WL (4050, 4120, 4200)
WO (2000, 2020, 2070)
SH (140, 220, 300)
SA (780, 850, 1000)
SL (450, 479, 600)
```
You can see it on the screen of the real app.
It is visible on top of the graph when `data` is switched on by the user.
The UI of the current real app looks like a Game.
There is not much space available to show text.
The Text goes into a TText control.

I have added an option in the published app
so that the user can show the log text in this component
instead of the Trimm-Item report.

Debugging the app on a device or inside of the simulator often does not work at all.
That is why it is valuable to have some old style *in app logging* capability built into the app.

> InApp logging is better for the user than InApp purchasing.

Please keep this in the new UI (to be done).

## Test Project

RG14.dpr is the test project.

Note that I added the `dproj` and the `res` file of the test project to `.gitignore`.

> You need to recreate private versions of those (use an new empty app).

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

The real application does all that, and it updates computed values for some of the coordinates on the fly,
according to the latest change to the value of the current parameter.

If you, as a developer, come up with a new implementation of the application shell,
I may be able to help with the completion of the new app, with some code from the original.

Until then you could use the published app (see Store) and provide data in the format expected.

## Questions and Answers

- I am working on this.
- It is not cast in stone.
- Find out in the log file what the application is doing.
- Read the source and step through the application code.

|   | Question |
| - | :------- |
| A | Should the app try and read Trimm-File automatically at application startup time? |
| B | Should the app try and read Trimm-File-Auto at application startup if Trimm-File does not exist? |
| C | Should reading a file strictly/always be a manual operation? |
| D | Should the app try and auto-save Trimm-File-Auto.txt at application shutdown time? |
| E | Should the app show standard Open/Save dialogs? |

We need a table detailing platform-rows and answer-columns:

| Platform        | A | B | C | D | E |
| :-------------- | - | - | - | - | - |
| Windows, normal | ? | ? | ? | ? | ? |
| Windows, appx   | 1 | 1 | 0 | 0 | 1 |
| Mac, App Store  | 1 | 1 | 0 | 0 | 1 |
| iPad            | 0 | 1 | 0 | 0 | 0 |
| Android         | 0 | 1 | 0 | 0 | 0 |

## Other

The Rggdoc.pas unit in folder Core is not used in the test project.
Maybe I should delete it from here soon.
But it may give you and idea of how it looked in the past.

## Links

- [RG on federgraph website](https://federgraph.de/trim-420.html)