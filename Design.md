# Design

We need to save and load data, but how should it be done?

- I am working on this.
- It is not cast in stone.
- Find out in the log file what the application is doing.
- Read the source and step through the application code.

## Questions and Answers

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
