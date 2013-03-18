/*  $RCSfile: cmdl.l,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:37 $  */
%{
/*****************************************************************************
 *
 *   cmdl.l:  lex input file for cmd
 *
 *****************************************************************************/

#include "cmdy.h"
extern int  yylval;

#define     MAXBUF  256
static char ArgBuf[MAXBUF], *ArgPointer;
static int  cmd;

#define retcmd(x) if (cmd) { cmd=0; return x; } else return NAME

cmdl_reset(argv)
    char *argv[];
{
    char **p, *s;

    ArgPointer = ArgBuf;
    cmd = 1;
    if (p = argv) {
        while (*(p+1)) p++;
        unput ('\n');
        do {
            unput(' ');
            s = *p + strlen(*p);
            while (--s >= *p) unput(*s);
        } while (--p >= argv);
    }
}

void ypush()
/*
 *  push token into argument buffer
 */
{
    if (ArgPointer+yyleng+1 >= ArgBuf+MAXBUF) {
        fprintf(stderr, " *** argument buffer overflow at '%s'\n", yytext);
        yylval = 0;
        return;
    }
    strcpy(ArgPointer, yytext);
    yylval = (int)ArgPointer;
    ArgPointer += yyleng + 1;
    return;
}

void ypushs()
/*
 *  push string into argument buffer (omit surrounding quotes)
 */
{
    if (ArgPointer+yyleng-1 >= ArgBuf+MAXBUF) {
        fprintf(stderr, " *** argument buffer overflow at '%s'\n", yytext);
        yylval = 0;
        return;
    }
    yytext[yyleng-1] = '\0';
    strcpy(ArgPointer, yytext+1);
    yylval = (int)ArgPointer;
    ArgPointer += yyleng - 1;
    return;
}

int     yywrap()        { return 1;}


%}

/*****************************************************************************/
/* lexical definitions
/*****************************************************************************/

%%

sites                           |
si                              { ypush(); retcmd(SITES);       }
list                            |
l                               { ypush(); retcmd(LIST);        }
group                           |
g                               { ypush(); retcmd(GROUP);       }
send                            |
s                               { ypush(); retcmd(SEND);        }
dump                            |
d                               { ypush(); retcmd(DUMP);        }
pr_dump                         |
pd                              { ypush(); retcmd(PR_DUMP);     }
rescan                          |
re                              { ypush(); retcmd(PR_RESCAN);   }
shutdown                        |
sd                              { ypush(); retcmd(SHUTDOWN);    }
snapshot                        |
snap                            { ypush(); retcmd(PR_SNAP);     }
help                            |
h                               |
"?"                             { ypush(); retcmd(HELP);        }
"+v"                            { ypush(); retcmd(PLUS_V);      }
"-v"                            { ypush(); retcmd(MINUS_V);     }
status                          |
st                              { ypush(); retcmd(STATUS);      }
quit                            |
q                               { ypush(); retcmd(QUIT);        }

[a-zA-Z_$][a-zA-Z0-9_$]*        { ypush(); return NAME;         }
("+"|"-")?[0-9]+                { ypush(); return NUMBER;       }

\"[^\"]*\"                      { ypushs(); return STRING;      }
\"[^\"\n]*/\n                   { printf(" *** missing \"\n"); return ERROR; }

":"                             { ypush(); return ':';          }
"@"                             { ypush(); return '@';          }
\n                              { ypush(); return CR;           }
";"                             { ypush(); cmd = 1; return ';'; }
[ \t]+                          ;
[^\":@\n; \t]+                  { ypush(); return ARG;          }

