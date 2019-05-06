# The Free Manga Downloader (FMD)

<sup>(Forked from https://github.com/riderkick/FMD)</sup>

## Download the latest release

[![Latest release](https://img.shields.io/github/release/fmd-project-team/FMD.svg)](https://github.com/fmd-project-team/FMD/releases/latest) [![Download latest release (Win32)](https://img.shields.io/github/downloads/fmd-project-team/FMD/latest/fmd_0.9.163.0.7z.svg?label=Win32)](https://github.com/fmd-project-team/FMD/releases/download/0.9.163.0/fmd_0.9.163.0.7z) [![Download latest release (Win64)](https://img.shields.io/github/downloads/fmd-project-team/FMD/latest/fmd_0.9.163.0_Win64.7z.svg?label=Win64)](https://github.com/fmd-project-team/FMD/releases/download/0.9.163.0/fmd_0.9.163.0_Win64.7z)
  
  
## Content

- [About FMD](#about-fmd)
- [Build instructions](#build-instructions)
- [Localization](#localization)

## About FMD

The Free Manga Downloader is a free open source application written in Object Pascal for managing and downloading manga from various websites. The source code was released under the GPLv2 license. FMD homesite is at https://github.com/riderkick/FMD or http://sf.net/p/newfmd.

## Build instructions

In order to build FMD from the source code, you must install the latest 1.8.x version of Lazarus (not 2.x.x) and Free Pascal Compiler from http://www.lazarus-ide.org/.  
[Currently used: `1.8.4`]  
  
Then you must install the following 3rd party Lazarus packages and components:

 - [RichMemo](https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/richmemo/) [Currently used: `r6815`]
 - [Virtual TreeView v4](https://github.com/blikblum/VirtualTreeView-Lazarus/tree/lazarus-v4) (and `lclextensions 0.6.1` from the Releases page) [Currently used: `4.8.7-R4`]
 - [Synapse](https://sourceforge.net/p/synalist/code/HEAD/tree/trunk/) [Currently used: `r209`]
 - [InternetTools](https://github.com/benibela/internettools) [Currently used: `git master commit af3cb6c5b010270ec9647fd22a5cd49bdb89a9c3 (29.01.2019)`]
 - [MultiLog](https://github.com/blikblum/multilog) [Currently used: `git master commit dac8373f485e4f8e20a41f6f7e7da298b48df0ab (15.10.2017)`]
 - [DCPCrypt](https://sourceforge.net/projects/lazarus-ccr/) [Currently used: `2.0.4.1`]

After everything is installed, open the file `md.lpi` by using Lazarus IDE. Make sure to add `ssl_openssl` to uses list of the `laz_synapse` package.
Then select `Run -> Build` to build the source code. If everything is ok, the binary file should be in `FMD_source_code_folder/bin`.

If InternetTools fail to compile (incompatible PPU), make sure to compile them individually first.

Some other external 3rd party tools and libraries are used:

 - [7-Zip Standalone Commandline Tool (7za)](https://www.7-zip.org) [Currently used: `19.00`]
 - [Duktape](https://github.com/grijjy/DelphiDuktape) [Currently used: `git master commit 61d8ce8cb9aa35e3168f4a32690cbd5e34c210b6 (01.03.2018)`]
 - [WebP (libwebp)](https://github.com/webmproject/libwebp/) [Currently used: `0.6.1`]
 - [Lua](http://luabinaries.sourceforge.net/) [Currently used: `5.3.x`]
 - [OpenSSL](https://www.openssl.org/) [Currently used: `1.0.2n`]
 - [SQLite](https://www.sqlite.org/) [Currently used: `3.22.0`]

These tools and libraries are not part of the source. You have to either download pre-compiled binaries, compile them yourself or just copy them from the latest FMD releases.

## Localization

Translations are stored inside `languages` folder with `.po` extension. In order to translate FMD to your native languages you can copy `fmd.po` and rename it to `fmd.xx.po`, where `xx` stand for two-letter language code. Additionally you can add country code at the end of language code. For reference you can look at http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes and http://en.wikipedia.org/wiki/ISO_3166-1. For example `id_ID` will be recognized as `Bahasa Indonesia (Indonesia)`. To translate the content of the file you need to use translation tools like [Poedit](https://poedit.net). Once you have finished translating all of its content you can launch FMD and it will automatically detect your new languages upon startup.
