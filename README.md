# TPocketSphinx
The required minimum set of [pocketpshinx](https://github.com/cmusphinx/pocketsphinxpocketsphinx) (and [sphinxbase](https://github.com/cmusphinx/sphinxbase)) headers, translated to Object Pascal. Custom thread wrapper `TPocketSphinx` is capable of recognizing English speech live from a default audio source in Windows and Linux, call back with status and hypothesis if available.

## Building examples
1. Demos: *colors* and *anyword* were designed in [Lazarus](http://www.lazarus-ide.org/) (version 1.6.0 used). Demo *anyword-delphi* was tested in Delphi XE10.
2. **Linux only**: pocketsphinx installation is explained in this [tutorial](http://cmusphinx.sourceforge.net/wiki/tutorialpocketsphinx). Basically `./configure` and `make install` for sphinxbase and pocketsphinx (in this order).
3. Build any of the examples:   
   `examples\colors\PocketSphinxColors.lpi` - Recognition of few words defined in grammar and dictionary files: *black, blue, green, red, yellow, white*. This demo will initialize very quickly and use minimum resources, since dictionary file contains only few words and search scope is limited to them.   
   `examples\anyword\PocketSphinxAnyWord.lpi` - Recognition of any word from a US English dictionary (~135k words), in Ngram language model search mode.   
   `examples\anyword-delphi\PocketSphinxDelphi.dproj` - Above demo but for Delphi   
   
   If building in Lazarus for Linux, change build configuration respectively (Windows configuration is default).
4. Download and unpack latest pocketsphinx ([pocketsphinx-5prealpha-win32.zip](https://sourceforge.net/projects/cmusphinx/files/pocketsphinx/5prealpha/pocketsphinx-5prealpha-win32.zip/download) originally used).   
Copy files:
   - all files and subdirectories `pocketsphinx\model\en-us` to `bin\en-us`
   Windows:
   - `pocketsphinx\bin\Release\Win32\pocketsphinx.dll` to `bin\pocketsphinx.dll`
   - `pocketsphinx\bin\Release\Win32\sphinxbase.dll` to `bin\sphinxbase.dll`
5. Make sure your recording device is set to microphone in system, since examples make use of default audio device.
6. Execute built demo and speak. If PocketSphinx fails to initialize - rebuild demo with Debug configuration, ps.log file is created with full log.   
In Windows if you get an error: "missing MSVCR110D.DLL", try installing Microsoft Visual C++ 2012 Redistributable in your system.

## Creating new grammar
*Colors* demo explains how to limit acceptable words via [JSpeech Grammar Format](https://www.w3.org/TR/jsgf/) file `bin\colors\colors.gram`  
To create a new list of "accepted" words that can be used in your "command and control" type of application:
   - Define words in grammar file, accordingly to your set of rules (words combine, repetitions etc.)
   - Create dictionary file with all of those words, and their pronunciation with tools listed below
   - Load dictionary and add new search with `LoadDictionary` and `AddGrammarSearchFile`
   
| Multiple words to pronunciation (.dic) file     | Single word to pronunciation                 |
| ----------------------------------------------- |----------------------------------------------|
| LOGIOS Lexicon Tool                             | The CMU Pronouncing Dictionary               |
| http://www.speech.cs.cmu.edu/tools/lextool.html | http://www.speech.cs.cmu.edu/cgi-bin/cmudict |

Make sure that words in your dictionary and grammar file are lowercase or uppercase for both (not mixed), otherwise PocketSphinx will fail to initialize.

## License
MIT