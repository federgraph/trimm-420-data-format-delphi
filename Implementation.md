# Implementation

My implementation is in the TRggData class, which is given in full, see code.

I am using several instances of that class to keep data in memory.

## TRggData instances

Just briefly, from the code, to make it clear:

```pascal
  TMain1 = class(TMain0)
  public
    RggMain: TRggMain; // RggMain.Rigg is the model of the app

    RggData: TRggData; // temp object for data transfer

    { slot used as reference for diffing }
    Trimm0: TRggData;

    { user data slots }
    Trimm1: TRggData; // selected with button T1
    Trimm2: TRggData; // selected with button T2
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;

    { example data slots }
    Trimm7: TRggData; // 420
    Trimm8: TRggData; // Logo
end;
```

- I wanted to allow the user of the app to deal with a limited number of Trimms.
- TRggData is holding a Trimm-Item in memory.
- T1-T6 are the user specified Trimms.
- T0 should hold a copy of the data in T1-T8
- Diffing is an important feature. You are diffing against T0.
- T7 and T8 are read only.
- T7 is the default in Trimm 420 app.
- T7 would be a little different in Trimm 470 app.
- RggData is is temporary object.

The data structure the real application (RG38) uses is part of class TRigg and is omitted from this test project.

## Notes

This test application deals with TRggData and uses a do nothing dummy implementation of TRigg.

A TRggData instance is referred to as a slot in this writing.

T1 is the caption of a button used to select Trimm1.

The six named Trimm-Items T1-T6 will be available for user selection, as the current Trimm.
When you select a Trimm, the data from that memory slot will be read,
and the current data of the application model in RggMain.Rigg will be overridden, lost without asking.

There are two more Trimm-Items, T7 and T8, which are very similar to T1-T6,
but which are reinitialized to default data when selected.
That is why they appear as read only.

Before you select a Trimm - and load data from a slot -
you may want to save the current model data back to the slot it was loaded from.
You do this by copy and paste,
which is certainly odd, but yes, copy and paste is an existing way to save data back to a slot.
This is valid for any of the slots, T1-T8.
The short caption of the button corresponding to the copy and save action is *cap*.

T0 is special, it is used for comparison only - it cannot be selected as the current Trimm -
but the UI of the real app can show the diff between current and T0.
T0 can be read from a Trimm-File, as the first Trimm-Item.

Using button **MT0** to update T0 with the current Rigg model data is an important action to be familiar with.
When you do this while the DiffText report is shown,
you can see how all the differences disappear,
you can watch how they become zero.

In the app, the user can move data from T1-T6 to current, and from current to TO.
The application will define actions to do this,
and these actions will be mapped to buttons or keyboard shortcuts.

Copy Trimm-Item will take the current state of Rigg, serialize it into Trimm-Item Text and copy as Text to the clipboard.

Pasting from clipboard is similar to loading a Trimm-File or Trimm-Item from a file, it will initialize one or more slots.
It is possible to move data from slot TX to slot TY, when you use the clipboard
and change currently selected slot between copy and paste operations.
Maybe in the future I will make it possible to copy between slots without going through the clipboard.

After a Trimm-Item was pasted from clipboard, the selected slot is updated, and the data is loaded into Rigg.
If the text that is on the clipboard was just copied there by the application, the state of Rigg will not change.

Using the buttons T1-T6, and T7 (420 default), and T8 (Logo example), should be an intuitive operation.
Data stored in the slot will be loaded into Rigg.

Using button cap (copy and paste) to write back current model data in Rigg to a slot is a thing which I have tried to explain.
We may need a more direct and intuitive way for the same, but it is not strictly needed.

When you have saved back current model changes from Rigg to a slot,
it still needs to be saved to disk in a Trimm-File when the application closes, or even earlier.

Note that this may or may not be a manual operation by default, you probably need to remember to save your work.
What the default mode of operation is for saving data is likely to be platform dependent.
You better check that out by reading the code.
I have published this test app so that you can.
Treat it as a good thing that there is no database, not at this time, not here in this code base.
Working with files is a challenge I have taken up here.
It is hard but doable. At least this is the assumption.

RggData is the instance of TRggData in TMain1 which is used as a temp object.
This one is used when we want to transfer the actual data to and from the real model object, the TRigg instance.
There are reports which print the state of RggData, Data and Json.
These reports do not change when you change the current model data of TRigg via the UI, with the mouse wheel.
So it appears as if those reports are pretty useless, perhaps only relevant while debugging.

You can watch the state within Rigg through the reports and graphics if implemented.
Here in the data format test project you can use the live reports DataText and JsonText to show the current Rigg state.

Here in the test app you can use Shift-Wheel or Ctrl-Wheel to change VOPos,
just so that you can see some change in the reports and identify the piece of data after saving.

Under normal circumstances any change to Rigg will immediately produce feedback through the graphics, which will change.
TRggData instances will only be effected when you initiate an IO operation.

ToDo: Unit tests for TRggData.

The current data in the Rigg model is usually not the same as the data in the slots,
which contain cached data from the time of loading, be it internal hardcoded data, a file from disk, or Text pasted from clipboard.

This repository has the MIT license, so that you can provide data in the format.

See repository RiggVar-RG38 (with GPL license) for real implementation of TRigg.
