#define _CRT_NONSTDC_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#include <io.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <sys/stat.h>

#define BITS 8

void main(int argc, const char *argv[])
{
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s file\n", argv[0]);
        return;
    }

    char name[256];

    sprintf(name, "%s.raw", argv[1]);
    int source = open(name, O_RDONLY | O_BINARY);
    fprintf(stderr, "source: %s\n", name);
    if (source == -1)
    {
        perror("source");
        return;
    }

    sprintf(name, "%s.a2stream", argv[1]);
    int target = open(name, O_WRONLY | O_BINARY | O_CREAT | O_TRUNC, S_IREAD | S_IWRITE);
    fprintf(stderr, "target: %s\n", name);
    if (target == -1)
    {
        close(source);
        perror("target");
        return;
    }

    int samples = 0;
    int size;
    float error = 0.0;
    bool last = false;
    do
    {
        float x[256 * BITS];
        size = read(source, x, sizeof(x)) / sizeof(float);
        if (size == -1)
        {
            perror("source");
            close(source);
            close(target);
            return;
        }

        unsigned char y[256];
        memset(y, 0, sizeof(y));

        // first-order delta-sigma converter
        for (int n = 0; n < size; n++)
        {
            bool next = x[n] >= error;
            error = (next ? 1.0 : -1.0) - x[n] + error;
            if (next != last)
            {
                y[n / BITS] |= 1 << (n % BITS);
            }
            last = next;
        }

        if (write(target, y, size / BITS) != size / BITS)
        {
            perror("target");
            close(source);
            close(target);
            return;
        }

        samples += size;
        int h = (samples / 78670 / 3600);
        int m = (samples / 78670 /   60) % 60;
        int s = (samples / 78670       ) % 60;
        fprintf(stderr, "offset: %02d:%02d:%02d\r", h, m, s);
    }
    while (size);

    close(source);
    close(target);
}
