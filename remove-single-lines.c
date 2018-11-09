#include <stdio.h>
#include <glob.h>

void simplify(int number)
{
    char filename[5000];
    snprintf(filename, 5000, "/home/dacoda/quicklisp/local-projects/goo-scraping/readings/%d", number);
    FILE* file = fopen(filename, "rb");
    int length;
    char * buffer;
    if (file)
    {
        fseek(file, 0, SEEK_END);
        length = ftell(file);
        fseek(file, 0, SEEK_SET);
        buffer = malloc(length);
        fread(buffer, 1, length, file);
    }
    fclose(file);

    for (int i = 0; i < length; ++i)
    {
        if (buffer[i] == ' ' || buffer[i] == '\n')
        {
            for (int j = i; j < length; ++j)
            {
                buffer[j] = buffer[j+1];
            }
            buffer[length-1] = '\0';
            --length;
            --i;
        }
    }
    buffer[length-9] = '\0';
    printf("%s\n", buffer);
}

int main(int argc, char* argv[])
{
    for (int i = 2; i < 295850; ++i)
    {
        simplify(i);
    }

    return 0;
}
