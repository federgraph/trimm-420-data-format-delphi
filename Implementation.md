# Implementation

My implementation is in the TRggData class, which is given in full, see code.

I am using several instances of that class to keep data in memory.

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
- T0 should hold a copy of T1-T8
- Diffing is an important feature. You are diffing against T0.
- T7 and T8 are read only.
- T7 is the default in Trimm420 app
- T7 would be a little different in Trimm470 app
- RggData is is temp object

The data structure the real application uses is part of class TRigg and is omitted from this test project.

## Side notes

The six named Trimm-Items T1-T6 will be available for user-selection, as the current Trimm.
When you select a Trimm, the data from the memory slot will be read,
and the current data of the single application model will be overridden, lost without asking.

There are two more Trimm-Items, T7 and T8, which are very similar to T1-T6,
but which are reinitialized to default data when selected.
That is why they appear as read only, while they are technically the same as any other TRggData instance.

A TRggData instance is referred to as a slot in this writing.

Before you select a Trimm, and load data from the slot,
you may want to save the data back to the slot, by copy and paste.
This is certainly odd, but copy and paste is a way to save data back to a slot,
any of the slots T1-T8.

T0 is special, it is for used comparison only - it cannot be selected as the current Trimm -
but the UI of the real app can show the diff between current and T0,
and T0 can be read from the Trimm-File, as the first Trimm-Item.

In the real app the user can move data from T1-T6 to current,
and from current to TO.
The application will define actions to do this,
and these actions will be mapped to buttons or keyboard shortcuts.

It is also possible to move data from TX to TY, when you use the clipboard
and change currently selected slot between copy and paste operations.

Pasting from clipboard is similar to loading a Trimm-Item from a file, it will initialize one or more slots.
It means that there is an existing way of writing to a slot.
Maybe in the future I will make it possible to copy between slots without going through the clipboard.

Using the buttons T1-T6, and T7 (420 default), and T8 (Logo example), should be intuitive.

Using button MT0 to update T0 with the current model data is important to know.
When you do this while the diff report is visible,
you can see how all the diffs disappear,
how they become zero.

Using button cap (copy and paste) to write back current model data to a slot is a thing which I have tried to explain.
We need an intuitive way  for the same.

And then, when you have saved back changes to the current model to a slot,
it still needs to be saved to disk in a Trimm-File,
when the application closes, or earlier.
Note that this may be a manual operation by default, you need to remember to save work.
What the default is for saving is likely to be platform dependent.
You better check that out by reading the code.

The good thing is that there is no database, not at this time, not here in this code base.

There is one more instance of TRggData in TMain1, which is used as a temp object.
This one is used when I want to transfer the actual data to and from the real model object, a TRigg instance.
This is mostly invisible to the outside, except that there are reports which show the content of it.
These reports would not change when you change the current model data of TRigg.
It appears as if those reports (Data and Json) are pretty useless, perhaps only relevant to debugging.
You should use the live reports (DataText and JsonText) to show the current data.

The current data in the model is not the same as the data in the slots,
except for the moment after loading from a slot.

This test application only deals with TRggData and has no real implementation of TRigg.
You can change VOPos with the wheel, just so that you can see some change and identify the piece of data.

This repository has the MIT license, so that you can provide data in the format.

See repository RiggVar-RG38 (with GPL license) for real implementation of TRigg.
