FMD
The Free Manga Downloader.

---------------------------------

Content:
	1.) About FMD
	2.) Build instructions
    3.) Localization

---------------------------------

FMD homesite is at http://sourceforge.net/projects/fmd/ or http://akarink.wordpress.com

---------------------------------

1.) About FMD

The Free Manga Downloader is a free open source application written in Object Pascal for managing and downloading manga from various websites. The source code was released under the GPLv2 license.

2.) Build instructions

In order to build FMD from the source code, you must install Lazarus latest version and Free Pascal Compiler v2.6.4. Then you must install the following 3rd party libraries and components:
  - RichMemo.
  - Virtual TreeView.
  - Synapse.
  - Vampyre Imaging Library.

After everything is installed, open the file md.lpi by using Lazarus IDE, select Run->Build to build the source code. If everything is ok, the binary file should be in FMD_source_code_folder/bin

3.) Localization

Translations are stored inside "languages" folder with .po extension. In order to translate FMD to your native languages you can copy "fmd.po" and rename it to "fmd.xx.po", where xx stand for two-letter language code (http://www.science.co.il/Language/Locale-codes.asp). For example "id_ID" will be recognized as "Bahasa Indonesia (Indonesia)". To translate the content of the file you need to use translation tools like Poedit. Once you have finished translating all of its content you can launch FMD and it will automatically detect your new languages upon startup.

---------------------------------

Last update 25-05-2015





