# Stream-PDM
Stream-PDM simultaneously receives Pulse-Density-Modulation Sound from any HTTP server and plays it through the stock Apple II speaker in a consistent 13 cycle loop resulting in a stable 78671.3Hz pulse frequency. Those 13 cycles are feasible through aggressive loop unrolling resulting in 32kB of code. Those 32kB aren't loaded from disk but are generated on the fly.

Hardware requirements:
* Enhanced //e or IIgs
* [Uthernet II](http://a2retrosystems.com/products.htm) in any slot

To built Stream-PDM:
* Get [cc65](https://cc65.github.io/)
* Get [IP65](https://github.com/cc65/ip65/wiki) and place it in a subdirectory `ip65`
* Enter `cl65 -t apple2enh -Or -I ip65 -D SINGLE_SOCKET stream-pdm.c stream.s w5100_http.c w5100.c linenoise.c ip65/ip65.lib ip65/ip65_apple2.lib`

To create a PDM file:
* Create a headerless ***.raw** file with 78670Hz mono 32-bit-float PCM data (e.g. with [Audacity](https://www.audacityteam.org/))
* Convert the ***.raw** file into an ***.a2stream** file with converter.exe

To stream a PDM file:
* Put the ***.a2stream** file onto any HTTP server
* Run Stream-PDM and point it to the URL of the ***.a2stream** file

To config Stream-PDM:
* Put the Uthernet II in slot 3 <ins>or</ins>
* Create a file named **ETHERNET.SLOT**. Only the first byte of that file is relevant. This byte can either represent your Uthernet II slot as binary value (e.g. $04 for slot 4), as ASCII digit (e.g. $34 for slot 4) or as Apple TEXT digit (e.g. $B4 for slot 4).
