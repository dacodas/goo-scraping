#include <stdio.h>
#include <glob.h>
#include <iostream>
#include <string>
#include <cstring>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>


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
        buffer = (char*) malloc(length);
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

int test(const char* target)
{
    // PCRE2_SPTR subject = reinterpret_cast<PCRE2_SPTR>(u8"いっ‐とう〔‐タウ〕【一党】");
    PCRE2_SPTR subject = reinterpret_cast<PCRE2_SPTR>(target);
    PCRE2_SPTR pattern = reinterpret_cast<PCRE2_SPTR>(u8"(.*)〔(.*)〕【(.*)】");

    int errorcode;
    PCRE2_SIZE erroroffset;
    pcre2_code* re = pcre2_compile(pattern, PCRE2_ZERO_TERMINATED, 
                                   PCRE2_ANCHORED | PCRE2_UTF, &errorcode,  
                                   &erroroffset, NULL);

    if ( re == NULL )
    {
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errorcode, buffer, sizeof(buffer));
        printf("PCRE2 compilation failed at offset %d: %s\n", (int)erroroffset,
               buffer);
        return 1;

    }

    pcre2_match_data* match_data = pcre2_match_data_create_from_pattern(re, NULL);
    int rc;

    rc = pcre2_match(
        re,                                 /* the compiled pattern */
        subject,                            /* the subject string */
        strlen(reinterpret_cast<const char*>(subject)), /* the length of the subject */
        0,                                  /* start at offset 0 in the subject */
        0,                                  /* default options */
        match_data,                         /* block for storing the result */
        NULL);                              /* use default match context */


    if (rc < 0)
    {
        switch(rc)
        {
            case PCRE2_ERROR_NOMATCH: printf("No match for %s\n", subject); break;
                /*
                  Handle other special cases if you like
                */
            default: printf("Matching error %d\n", rc); break;
        }
        pcre2_match_data_free(match_data);   /* Release memory used for the match */
        pcre2_code_free(re);                 /* data and the compiled pattern. */
        return 1;
    }

/* Match succeded. Get a pointer to the output vector, where string offsets are
   stored. */

    PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
    printf("Match succeeded at offset %d\n", (int)ovector[0]);

/*************************************************************************
 * We have found the first match within the subject string. If the output *
 * vector wasn’t big enough, say so. Then output any substrings that were *
 * captured.                                                              *
 *************************************************************************/

/* The output vector wasn’t big enough. This should not happen, because we used
   pcre2_match_data_create_from_pattern() above. */

    if (rc == 0)
        printf("ovector was not big enough for all the captured substrings\n");

/* We must guard against patterns such as /(?=.\K)/ that use \K in an assertion
   to set the start of a match later than its end. In this demonstration program,
   we just detect this case and give up. */

    if (ovector[0] > ovector[1])
    {
        printf("\\K was used in an assertion to set the match start after its end.\n"
               "From end to start the match was: %.*s\n", (int)(ovector[0] - ovector[1]),
               (char *)(subject + ovector[1]));
        printf("Run abandoned\n");
        pcre2_match_data_free(match_data);
        pcre2_code_free(re);
        return 1;
    }

/* Show substrings stored in the output vector by number. Obviously, in a real
   application you might want to do things other than print them. */

    for (int i = 0; i < rc; i++)
    {
        PCRE2_SPTR substring_start = subject + ovector[2*i];
        size_t substring_length = ovector[2*i+1] - ovector[2*i];
        printf("%2d: %.*s\n", i, (int)substring_length, (char *)substring_start);
    }



    return 0;
}

int main(int argc, char* argv[])
{
    // for (int i = 2; i < 295850; ++i)
    // {
    //     simplify(i);
    // }

    std::string line;
    while (std::getline(std::cin, line))
    {
        test(line.c_str());
    }


    return 0;
}

