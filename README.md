# Free Manga Downloader (FMD)

<sup>(Forked from https://github.com/riderkick/FMD)</sup>

## Download

[![Latest release](https://img.shields.io/github/release/fmd-project-team/FMD.svg)](https://github.com/fmd-project-team/FMD/releases/latest)
[![Download latest release (Win32)](https://img.shields.io/github/downloads/fmd-project-team/FMD/latest/fmd_0.9.167.2.7z.svg?label=Win32)](https://github.com/fmd-project-team/FMD/releases/download/0.9.167.2/fmd_0.9.167.2.7z)
[![Download latest release (Win64)](https://img.shields.io/github/downloads/fmd-project-team/FMD/latest/fmd_0.9.167.2_Win64.7z.svg?label=Win64)](https://github.com/fmd-project-team/FMD/releases/download/0.9.167.2/fmd_0.9.167.2_Win64.7z)

## About FMD

This is an active fork of the Free Manga Downloader which is a free open source application written in Object Pascal for managing and downloading manga from various websites. The source code was released under the GPLv2 license.  
  
  
[Supported Websites](https://github.com/fmd-project-team/FMD/wiki/Supported-Websites)  
  
## Build instructions

In order to build FMD from the source code, you must install the latest 1.8.x version of Lazarus (not 2.x.x) and Free Pascal Compiler:  
[![Lazarus IDE 1.8.4](https://img.shields.io/badge/Lazarus%20IDE-1.8.4-Blue.svg)](http://www.lazarus-ide.org/)  

Then you must install the following 3rd party Lazarus packages and components:  
[![RichMemo r6815](https://img.shields.io/badge/RichMemo-r6815-Blue.svg)](https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/richmemo/)  
[![Virtual TreeView 4.8.7-R4](https://img.shields.io/badge/Virtual%20TreeView-4.8.7--R4-Blue.svg)](https://github.com/blikblum/VirtualTreeView-Lazarus/tree/lazarus-v4)  
[![LCL Extensions 0.6.1](https://img.shields.io/badge/LCL%20Extensions-0.6.1-Blue.svg)](https://github.com/blikblum/VirtualTreeView-Lazarus/releases/download/lazarus-4.8.7-R4/lclextensions-0.6.1.zip)  
[![Synapse r209](https://img.shields.io/badge/Synapse-r209-Blue.svg)](https://sourceforge.net/p/synalist/code/HEAD/tree/trunk/)  
[![InternetTools](https://img.shields.io/badge/InternetTools-git%20master%20commit%20af3cb6c5b010270ec9647fd22a5cd49bdb89a9c3%20(29.01.2019)-Blue.svg)](https://github.com/benibela/internettools)  
[![MultiLog](https://img.shields.io/badge/MultiLog-git%20master%20commit%20dac8373f485e4f8e20a41f6f7e7da298b48df0ab%20(15.10.2017)-Blue.svg)](https://github.com/blikblum/multilog)  
[![DCPCrypt](https://img.shields.io/badge/DCPCrypt-2.0.4.1-Blue.svg)](https://sourceforge.net/projects/lazarus-ccr/)  

After everything is installed, open the file `md.lpi` by using Lazarus IDE. Make sure to add `ssl_openssl` to uses list of the `laz_synapse` package.
Then select `Run -> Build` to build the source code. If everything is ok, the binary file should be in `FMD_source_code_folder/bin`.

If InternetTools fail to compile (incompatible PPU), make sure to compile them individually first.

Some other external 3rd party tools and libraries are used:  
[![7-Zip](https://img.shields.io/badge/7--Zip%20(Standalone)-19.00-Blue.svg)](https://www.7-zip.org)  
[![Duktape](https://img.shields.io/badge/Duktape-git%20master%20commit%2061d8ce8cb9aa35e3168f4a32690cbd5e34c210b6%20(01.03.2018)-Blue.svg)](https://github.com/grijjy/DelphiDuktape)  
[![WebP (libwebp)](https://img.shields.io/badge/WebP%20(libwebp)-0.6.1-Blue.svg)](https://github.com/webmproject/libwebp/)  
[![Lua](https://img.shields.io/badge/Lua-5.3-Blue.svg)](http://luabinaries.sourceforge.net/)  
[![OpenSSL](https://img.shields.io/badge/OpenSSL-1.0.2n-Blue.svg)](https://www.openssl.org/)  
[![SQLite](https://img.shields.io/badge/SQLite-3.22.0-Blue.svg)](https://www.sqlite.org/)  
[![cfscrape](https://img.shields.io/badge/cfscrape-2.0.8.1-Blue.svg)](https://github.com/Anorov/cloudflare-scrape/) <sup>(Compiled with PyInstaller included in cf_bypass.exe)</sup>  
[![Node.js](https://img.shields.io/badge/Node.js-12.8.0.0-Blue.svg)](https://nodejs.org/)  

These tools and libraries are not part of the source. You have to either download pre-compiled binaries, compile them yourself or just copy them from the latest FMD releases.

## Localization

Translations are stored inside `languages` folder with `.po` extension. In order to translate FMD to your native languages you can copy `fmd.po` and rename it to `fmd.xx.po`, where `xx` stand for two-letter language code. Additionally you can add country code at the end of language code. For reference you can look at http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes and http://en.wikipedia.org/wiki/ISO_3166-1. For example `id_ID` will be recognized as `Bahasa Indonesia (Indonesia)`. To translate the content of the file you need to use translation tools like [Poedit](https://poedit.net). Once you have finished translating all of its content you can launch FMD and it will automatically detect your new languages upon startup.
