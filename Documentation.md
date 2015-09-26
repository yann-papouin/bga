#One-page Documentation

    Introduction and first launch
    Recent opened/saved file list
    Param read support
    File manipulation
    Adding new files
        Drag&drop support
        Packing
    Save, Edit and Defrag
    Find bar
    Standard mesh preview
    Map preview
    File edition

Introduction and first launch

This content is a One-page documentation for BGA, all features are explained here. The first time you launch BGA, the window is empty because you directly start a new RFA file by adding new files, the next time it will automatically display the last file opened if it exists again.

Recent opened/saved file list

After each open/save operation, the .rfa file is automaticaly added to the top of the recent list. It allows us to quicky re-open last archives.

Param read support

BGA is a WinRFA replacement tool, all WinRFA features are implemented in BGA and the best one, that required only one line of code and is missing from WinRFA, is the ability to open RFA archives directly from the Operating System Shell (in most of cases it will be Microsoft Windows). A double-clic on a .rfa allow to open it in BGA, but first you need to associate .rfa with bga.exe, and you just have to run the "Associate RFA files with BGA" command that you can find in the "Help->Tools" menu. This command that requires administrator rights on Vista/Seven will be automatically run as it.

As you can see below, .rfa files are now associated with BGA.

File manipulation

BGA allow importation, modification, deletion of any files. All these operations are asynchronous because they required a lot of time (in terms computer time), so they are executed during a save operation.

To be easily identified by the user, "importation" and "modification" are colored:

    Importation (File is highlighted in Green)
    Modification (File is highlighted in Orange)
    Deletion (File is not visible anymore) 

If you want to learn more about RFA file format you can read an explanation at: http://code.google.com/p/bga/wiki/Fragmentation#RFA_file_structure
Adding new files
Drag&drop support

Packing

This feature is directly inherited from WinRFA and allow level packing.

Save, Edit and Defrag

To correctly understand the defrag operation, you can read an explanation at: http://code.google.com/p/bga/wiki/Fragmentation#RFA_file_fragmentation

Find bar

Standard mesh preview

Map preview

File edition
