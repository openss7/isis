/*  $RCSfile: cmdy.y,v $ $Revision: 2.0 $ $Date: 90/05/04 15:23:40 $  */
/*
 * ISIS System V1.0 (8/15/88).   Report problems to isis-bugs@gvax.cs.cornell.edu 
 * Note: Export restrictions apply.  License required for export to ``non-free world''
 * countries (USDC terminology).  Agreement to respect this export restriction required
 * for export to all other countries.
 */
/*****************************************************************************
 * cmdy.y - yacc input for cmd
 *
 *****************************************************************************/

%token SITES LIST GROUP SEND DUMP PR_DUMP PR_SNAP PR_RESCAN SHUTDOWN HELP 
%token PLUS_V MINUS_V STATUS QUIT NAME NUMBER STRING CR ARG ERROR

%start input

%{
#include <stdio.h>
extern  int  optv;

#define  MAXARG  64
static  char *argv[MAXARG], **argp;
static  char buf[80];

cmdy_reset()
{
    argp = argv;
}

int  PushArg(a)
  int  a;
{
    if (argp >= argv + MAXARG) {
        fprintf(stderr, " *** too many arguments\n");
        return 0;
    }
    *argp = (char *)a;
    return (int)argp++;
}


%}

%%
/*****************************************************************************/
/* syntactical definitions
/*****************************************************************************/


input       : CR                        { return 0; }
            | CommandList CR            { return 0; }
            | error CR                  { return 1; }
            ;

CommandList : command
            | CommandList ';' command
            ;

command     : SITES                     { CmdSites();           }
            | LIST                      { CmdList( 0,  0);      }
            | LIST   scope              { CmdList($2,  0);      }
            | LIST   scope ':' gname    { CmdList($2, $4);      }
            | LIST   gname              { CmdList( 0, $2);      }
            | GROUP  group              { CmdGroup($2);         }
            | SEND   adr  ArgList       { CmdSend($2, $3);      }
            | DUMP                      { CmdDump();            }
            | PR_DUMP                   { CmdPrdump();          }
            | PR_SNAP                   { CmdPrsnap();          }
            | PR_RESCAN                 { CmdRescan();          }
            | SHUTDOWN                  { CmdShutdown();        }
            | HELP                      { CmdHelp(0);           }
            | HELP   arg                { CmdHelp($2);          }
            | PLUS_V                    { optv = 1;             }
            | MINUS_V                   { optv = 0;             }
            | STATUS                    { CmdStatus();          }
            | QUIT CR                   { CmdQuit();            }
            ;

site        : NUMBER
            ;
scope       : '@' NAME                  { $$ = $2; }
            | '@' STRING                { $$ = $2; }
            | '@' ARG                   { $$ = $2; }
            ;
gname       : NAME | STRING | ARG
            ;
group       : gname
            | scope ':' gname           { $$ = (int) sprintf(buf, "@%s:%s", $1, $3); }
            ;
pid         : NUMBER
            ;
arg         : NAME | NUMBER | STRING | ARG
            ;
ArgList     : args                      { $$ = $1; PushArg(0);  }
            ;
args        : arg                       { $$ =     PushArg($1); }
            | args arg                  { $$ = $1; PushArg($2); }
            ;

adr         : site ':' group ':' pid    { $$ = MakeAlist($1, $3, $5);   }
            | site ':' group            { $$ = MakeAlist($1, $3,  0);   }
            |          group ':' pid    { $$ = MakeAlist( 0, $1, $3);   }
            |          group            { $$ = MakeAlist( 0, $1,  0);   }
            | site                      { $$ = MakeAlist($1,  0,  0);   }
            |      ':' pid              { $$ = MakeAlist( 0,  0, $2);   }
            | site ':' pid              { $$ = MakeAlist($1,  0, $3);   }
            ;

%%


/*** print error message */
void    yyerror(s)
        char    *s;
        
{ extern  char    yytext[];

  printf(" *** %s at or before '%s'\n", s, *yytext=='\n'? "\\n": yytext);
  return;
}

