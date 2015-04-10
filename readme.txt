FMD
The Free Manga Downloader.

---------------------------------

Content:
	1.) About FMD
	2.) Build instructions

---------------------------------

FMD homesite is at http://sourceforge.net/projects/fmd/ or http://akarink.wordpress.com

---------------------------------

1.) About FMD

The Free Manga Downloader is a free open source application written in Object Pascal for managing and downloading manga from various websites. The source code was released under the GPLv2 license.

2.) Build instructions

In order to build FMD from the source code, you must install Lazarus latest version and Free Pascal Compiler v2.6.2. Then you must install the following 3rd party libraries and components:
  - RichMemo.
  - Virtual TreeView.
  - Synapse.
  - Vampyre Imaging Library.
  - ExceptionLogger

For Synapse and Vampyre Imaging Library, you need to put them all in FMD_source_code_folder/trunk/3rd

After everything is installed, open the file md.lpr by using Lazarus IDE, select Run->Build to build the source code. If everything is ok, the binary file should be in FMD_source_code_folder/trunk/bin

---------------------------------

Last update 08-30-2014





