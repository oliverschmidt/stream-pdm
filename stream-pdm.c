#include <ctype.h>
#include <fcntl.h>
#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <ip65.h>

#include "w5100.h"
#include "w5100_http.h"
#include "linenoise.h"

void streamafterexit(uint8_t eth_init);

void error_exit(void)
{
  printf("- %s\n", ip65_strerror(ip65_error));
  exit(EXIT_FAILURE);
}

void confirm_exit(void)
{
  printf("\nPress any key ");
  cgetc();
}

bool match(const char *filter, const char *string)
{
  while (*filter)
  {
    if (!*string)
    {
      return false;
    }
    if (toupper(*filter++) != toupper(*string++))
    {
      return false;
    }
  }
  return true;
}

void completion(const char *line, linenoiseCompletions *lc)
{
  if (match(line, "http://"))
  {
    linenoiseAddCompletion(lc, "http://");
  }
  if (match(line, "http://www."))
  {
    linenoiseAddCompletion(lc, "http://www.");
  }
}

void main(void)
{
  uint8_t eth_init = ETH_INIT_DEFAULT;
  char *url;

  videomode(VIDEOMODE_80COL);

  {
    int file;

    printf("\nSetting slot ");
    file = open("ethernet.slot", O_RDONLY);
    if (file != -1)
    {
      read(file, &eth_init, 1);
      close(file);
      eth_init &= ~'0';
    }
  }

  printf("- %d\n\nInitializing %s ", eth_init, eth_name);
  if (ip65_init(eth_init))
  {
    error_exit();
  }

  // Abort on Ctrl-C to be consistent with Linenoise
  abort_key = 0x83;

  printf("- Ok\n\nObtaining IP address ");
  if (dhcp_init())
  {
    error_exit();
  }
  printf("- Ok\n\n");

  linenoiseHistoryLoad("stream.urls");
  while (true)
  {
    linenoiseSetCompletionCallback(completion);

    url = linenoise("URL? ");
    if (!url || !*url)
    {
      putchar('\n');
      return;
    }

    linenoiseHistoryAdd(url);

    printf("\n\nProcessing URL ");
    if (!url_parse(url))
    {
      break;
    }

    printf("- %s\n\n", ip65_strerror(ip65_error));
  }

  linenoiseHistorySave("stream.urls");
  linenoiseHistoryReset();
  printf("- Ok\n\n");

  // Copy IP config from IP65 to W5100
  w5100_config(eth_init);

  {
    char *http = malloc(0x1000);

    if (!http || !w5100_http_open(url_ip, url_port, url_selector, http, 0x1000))
    {
      return;
    }

    free(http);
  }

  clrscr();
  gotoxy(0, 20);
  cprintf("%.160s", url);
  gotoxy(0, 23);
  cprintf("Initializing...");
  gotoxy(60, 23);
  cprintf("Oliver Schmidt, 2020");

  {
    int y;

    // Switch to split screen with low res graphics
    *(char*)0xC056 = 0;
    *(char*)0xC053 = 0;
    *(char*)0xC050 = 0;

    // Clear graphics part of screen
    for (y = 0; y < 20; ++y)
    {
      gotoy(y);
      memset(*(char**)0x28, 0x00, 40);
    }

    // Show upper divider line
    gotoy(0);
    memset(*(char**)0x28, 0x0F, 40);

    // Show big arrow consisting of dark blue, blue, light blue and white
    for (y = 0; y < 15; ++y)
    {
      int x;
      int d = 8 - abs(7 - y);

      gotoy(2 + y);
      for (x = 0; x < 4; ++x)
      {
        uint8_t color[] = {0x22, 0x66, 0x77, 0xFF};

        memset(*(char**)0x28 + d + (x * 8), color[x], 8);
      }
    }

    // Show lower divider line
    gotoy(18);
    memset(*(char**)0x28, 0xF0, 40);
  }

  // Streaming overwrites almost all of the C program so do first a clean
  // shutdown of the C program and initialize the streaming afterwards
  streamafterexit(eth_init);
}
