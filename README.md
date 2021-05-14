# Free Pascal / Lazarus Po Files Utilities

Ten command line utilites to modify `.po` translation files used by Free Pascal and Lazarus.

These tools are similar to utilities available for [GNU/Gettext](https://www.gnu.org/software/gettext/) and it's Delphi version [dxgettext](https://sourceforge.net/projects/dxgettext/) with the difference that they are adapted to the `.po` dialect used in Free Pascal / Lazarus.

**Warning**: The author offers no guarantee of any sort with regard to these tools. However an effort has been made to avoid loosing any work. To that end, if a file is to be overwritten, an attempt is made to backup the original file.


<!-- TOC -->

- [1. poclean](#1-poclean)
- [2. pocopy](#2-pocopy)
- [3. pofill](#3-pofill)
- [4. poginore](#4-poginore)
- [5. poinfo](#5-poinfo)
- [6. pomerge](#6-pomerge)
- [7. posort](#7-posort)
- [8. postrip](#8-postrip)
- [9. poswap](#9-poswap)
- [10. poupdate](#10-poupdate)

<!-- /TOC -->

## 1. poclean

Removes all duplicate entries in a `.po` file. 

    usage:
        poclean source[.po] [output[.po]]


## 2. pocopy

Copies all msgid to msgstr in a `.po` file

    usage:
        pocopy source[.po] [output[.po]]

Note: The typical first entry in a `.po` file

    msgid ""
    msgstr "Content-Type: text/plain; charset=UTF-8"

is not modified by this utility. However, should the first entry begin with an entity, such as  

    #: form1.label8.caption

then msgstr will be set equal to msgid.


## 3. pofill

Fills any empty msgstr field a `.po` file with the content of the corresonding msgid.

    usage:
        pofill sourcefile[.po] [outputfile[.po]]

## 4. poginore

Removes all entries found in a remove `.po` file from a source `.po` file

    usage:
        poignore source[.po] remove[.po] [output[.po]]


## 5. poinfo

Provides information about a `.po` file

    usage:
        poinfo source[.po]

The utility reports errors while reading the source file, then displays summary statistics and ends with a list of entries that have a duplicate entity (which should never be allowed),
or a duplicate msgid or a duplicate msgstr. Empty msgstr are not considered duplicates.

Example output:

    $ ./poinfo bad
        1  Error: Missing leading " quote in msgstr in line 17
        2  Error: Entry 3 (stringres.mssconnected) does not have a msgstr in line 19
        ...
        8  Error: Entry 12 (stringres.smessagesent) does not have a msgstr in line 61
        9  Error: Entity empty in line 66

    Source: bad`.po`
    Entries: 54
    Errors: 9
    Fuzzys: 0
    Duplicate entities: 3
    Duplicate msgid: 3
    Duplicate msgstr: 1

    Entry 6 () has a duplicate entity
    ... 
    Entry 47 (tbrokereditform.label2.caption) has a duplicate msgstr

The error checking is by no means exhaustive.

## 6. pomerge

Merges two `.po` files

    usage:
        pomerge source[.po] more[.po] [output[.po]]

The output `.po` file is constructed as follows. 

-  All entries in more are copied to it. 
-  All entries in more with an entity not in source are added to output. 
-  If an entry in more has an entitity that is also in source then
   - if the msgid and msgstr of both entries are the same, the entry is not added to output and it is removed from more (there is no conflict). 
   - if the msgid and msgstr of both entires are different, the entry is not added to output and if is kept in more (there is a conflict).
- At the end all remaining entries in more are saved under the name more`.po`.conflicts    

## 7. posort

Sorts a `.po` file by entity

    usage:
        posort source[.po]  [output[.po]]

It is not mandatory to sort entries alphabetically, but the Lazarus IDE does generate sorted `.po` files. <!-- This will not remove duplicate entries; for that see poclean. -->

## 8. postrip

Removes all translated strings in a `.po` file

    usage:
        stripmsgstr source[.po] [output[.po]]

The first entry in the output `.po` file will be

    msgid ""
    msgstr "Content-Type: text/plain; charset=UTF-8"

No matter what the source contains.

A good way to generate a template.        

## 9. poswap

Exchanges all msgid and msgstr in a `.po` file

    usage:
        stripmsgstr source[.po] [output[.po]]

If the msgstr field on an entry is empty, then the msgid field will not be changed.

Typically the first entry in the source file will define the content type as follows.

    msgid ""
    msgstr "Content-Type: text/plain; charset=UTF-8"

The msgid and msgstr of such an entry will not be exchanged.

## 10. poupdate

Updates empty msgstr in a `.po` file from translations found in another `.po` file

    usage:
        poupdate source[.po] trans[.po] [output[.po]]