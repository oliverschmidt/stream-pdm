# Stream-PDM
Stream Pulse-Density-Modulation Sound on the Apple II

To built Stream-PDM:
* Get [cc65](https://cc65.github.io/)
* Get [IP65](https://github.com/cc65/ip65/wiki) and place it in a subdirectory `ip65`
* Enter `cl65 -t apple2enh -Or -I ip65 -D SINGLE_SOCKET stream-pdm.c stream.s w5100_http.c w5100.c linenoise.c ip65/ip65.lib ip65/ip65_apple2.lib`

