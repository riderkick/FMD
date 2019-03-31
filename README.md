# The Free Manga Downloader (FMD)

<sup>(Forked from https://github.com/riderkick/FMD)</sup>

## Download the latest release

[![Latest release](https://img.shields.io/github/release/SDXC/FMD.svg)](https://github.com/SDXC/FMD/releases/latest) [![Download latest release (Win32)](https://img.shields.io/github/downloads/SDXC/FMD/latest/fmd_0.9.160.0.7z.svg?label=Win32)](https://github.com/SDXC/FMD/releases/download/0.9.160.0/fmd_0.9.160.0.7z) [![Download latest release (Win64)](https://img.shields.io/github/downloads/SDXC/FMD/latest/fmd_0.9.160.0_Win64.7z.svg?label=Win64)](https://github.com/SDXC/FMD/releases/download/0.9.160.0/fmd_0.9.160.0_Win64.7z)

## Public Announcement

Hi everyone

From today onwards I will take over the FMD project. For the time being, I will focus on the fast and important fixes, so most of you can keep using FMD. Support from the community is appreciated (by contributing lua fixes for example).


## Content

- [About FMD](#about-fmd)
- [Build instructions](#build-instructions)
- [Localization](#localization)

## About FMD

The Free Manga Downloader is a free open source application written in Object Pascal for managing and downloading manga from various websites. The source code was released under the GPLv2 license. FMD homesite is at https://github.com/riderkick/FMD or http://sf.net/p/newfmd.

## Build instructions

In order to build FMD from the source code, you must install the latest version of Lazarus and Free Pascal Compiler from http://www.lazarus-ide.org/. Then you must install the following 3rd party libraries and components:

 - [RichMemo](https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/richmemo/)
 - [Virtual TreeView](https://github.com/blikblum/VirtualTreeView-Lazarus/tree/lazarus-v4) (and `lclextensions` from the Releases page)
 - [Synapse](https://sourceforge.net/p/synalist/code/HEAD/tree/trunk/) (at least revision `r160`)
 - [InternetTools](https://github.com/benibela/internettools) 
 - [MultiLog](https://github.com/blikblum/multilog)
 - [DCPCypt](https://sourceforge.net/projects/lazarus-ccr/)

After everything is installed, open the file `md.lpi` by using Lazarus IDE. Make sure to add `ssl_openssl` to uses list of the `laz_synapse` package.
Then select `Run -> Build` to build the source code. If everything is ok, the binary file should be in `FMD_source_code_folder/bin`.

If InternetTools fail to compile (incompatible PPU), make sure to compile them individually first.

## Localization

Translations are stored inside `languages` folder with `.po` extension. In order to translate FMD to your native languages you can copy `fmd.po` and rename it to `fmd.xx.po`, where `xx` stand for two-letter language code. Additionally you can add country code at the end of language code. For reference you can look at http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes and http://en.wikipedia.org/wiki/ISO_3166-1. For example `id_ID` will be recognized as `Bahasa Indonesia (Indonesia)`. To translate the content of the file you need to use translation tools like [Poedit](https://poedit.net). Once you have finished translating all of its content you can launch FMD and it will automatically detect your new languages upon startup.
