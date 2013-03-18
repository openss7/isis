/*   $RCSfile: pmklib.c,v $ $Revision: 2.1 $ $Date: 90/07/31 16:18:20 $ */
/*
 * pmake
 * pmklib.c
 * modified by Mark J. Steiglitz, 10/26/89
 */

/*
    This program enqueues a bourne shell command to be executed later by
    pexec as part of a parallel make.  All of the information necessary to
    characterize and execute the command is appended to a file which 
    accumulates descriptions of all of the commands to be executed during
    one invokation of pexec.

    The run string parameters are as follows:
     argv[1] : name of the file in which the graph is being accumulated.
                     ( used only by pmksh in shell driven mode )
     argv[2] : S for save environment, otherwise use previous env.
     argv[3] : dependency description
     argv[4] and remainder : shell command to be executed

     The maximum size of the run string supported is 4095 characters.

     The dependency description consists of a quoted string of the form:
     INFILE INFILE .... INFILE > OUTFILE > OUTFILE
     where INFILE is an input dependency and OUTFILE is an output dependency.

     The information needed by pexec to properly manage execution includes
     a complete list of the input and output file locations, input file sizes,
     and estimates of the execution time of the command and output file sizes.
     Make scripts do not include all of this information in that:
     
     1- not all INFILES or OUTFILES listed are actually files.  They may 
        represent keywords defining activity alternatives or names representing
        abstract dependencies which do not involve files.  If an input file
        does not exist it is recorded as a dependency with size=-1.

     2- the directory which contains a file referenced by a dependency may
        not be the current directory.  In fact the directories searched
        for some file names depends on the file type, typically represented
        by the file extension.  Thus command specific information is 
        necessary to locate all of the files referenced by the dependency list.

     3- The output file size, directory, and execution time are directly
        dependent on the command(s) executed.

     The activity of pmksh consists of:
      identify the command(s) to be executed
      using code specific to the command:
        locate input file directories, sizes, and output directory
        estimate output file size and execution time
        append the derived description of the step to the graph file
      
     If the command(s) in the script are not recognized the output file
     size is assumed to be -1 ( non-existent ) and the execution time
     is assumed to be UNIT_TIME, an arbitrary constant.
     
     Command specific code is chosen from a library of routines based on the
     first word of the command with any path name componants stripped off.
     Each entry accepts pmksh's run string description and a parameter 
     indicating the location of the command in the arguement string, and is
     expected to post input, output, and time descriptions to the
     graph description file.


     IMPLEMENTATION NOTE:  currently only the first word in the command
                           (argv[4]) is examined in characterizing it.

     The following routines reside in pmklib.o and are available for use in
     command specific library routines which reside here.

      append_cmd - append the command indicated by pmklib's arguements
                   to the file opened in append mode as the stream indicated
                   by the first parameter.

      append_in - append an input file name and size to the opened file
      append_out - append an output file name and size to the opened file
      append_time - append expected execution time to the opened file
      append_env - append environment dump to the opened file

      append_end - append the step end indicator to the opened file

      get_file_sz - return the number of characters in the indicated file
      get_dep_word - obtain the next word of the dependency string

      lin_suff_dep - evaluate dependencies using linear contributions to
                     execution time and output size based on file name suffix.
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>

#include <pmkdat.h>

#define UNIT_TIME 5000     /* number of milliseconds for unrecog. cmd. */

/* enumerated type constants for get_dep_word word type */
#define IN_DEP_FILE 1
#define OUT_DEP_FILE 2
#define CHAR_STRING 3
#define DEP_DONE 0

extern char **environ;

/****************************************************************************/

/*  DEPENDENCY STRING PARSER  */
/*
   dependency string format:
   { {infile}* {>outfile [char_string]}*;}*
   {...}* - repeated construct - 0 or more
   [...]  - optional construct - 0 or 1
   blanks delimit infile and outfile
   blanks insignificant in char_string - delimited by unescaped > or ;

*/

/* the parser is implemented by a simple automaton.  Entries for the current
   state are scanned until an entry for the next input character is found.
   The action and next state in that entry control subsequent activity.
   The last entry each state is the default action for that state.
   
   The RTN actions save the last character scanned, and rescan it on
   next call. */

/* scan states: state number is index of first table entry for that state. */
#define START_IN 0 /* look for beginning of input word */
#define ACCUM_IN 5 /* accumulate input word */
#define START_OUT 10  /* look for beginning or output word */
#define ACCUM_OUT 15  /* accumulate output word */
#define START_CHAR 20 /* look for start of characterization string */
#define ACCUM_CHAR 25 /* accumulate characterization string */
#define LAST_STATE 30 /* index of last array element */

/* actions */
#define START_WORD 1 /* set return value of p_word to indicate start of word */
#define RTN_IN 2     /* return an input word */
#define RTN_OUT 3    /* return an output word */
#define RTN_CHAR 4   /* return a characterization string */
#define RTN_DONE 5   /* end of string */
#define ACCUM_WORD 6 /* add the character to the word being accumulated */
#define ACCUM_ESC_WORD 7 /* add 2 characters to the word being accum. */
#define SKIP_CHAR 8  /* skip the character */

/* automaton definition */
typedef struct {int entry_state; /* current state represented */
                char see[1];     /* input observed */
                int action;      /* action to take */
                int next_state;  /* state to go to */
               } automatype;

automatype dep_grammar[LAST_STATE+1]={
  START_IN,' ',SKIP_CHAR,START_IN,    /* skip spaces during infile */
  START_IN,'>',SKIP_CHAR,START_OUT,   /* > switches to output */
  START_IN,';',SKIP_CHAR,START_IN,    /* ; in input just restarts input */
  START_IN,'\0',RTN_DONE,START_IN,    /* return end of input */
  START_IN,'A',START_WORD,ACCUM_IN,   /* default - accumulate input */

  ACCUM_IN,' ',RTN_IN,START_IN,       /* space ends input, expect more */
  ACCUM_IN,';',RTN_IN,START_IN,       /* same with ; */
  ACCUM_IN,'>',RTN_IN,START_OUT,      /* > ends inputm expect output */
  ACCUM_IN,'\0',RTN_IN,START_IN,      /* \0 ends input, report done next */
  ACCUM_IN,'A',ACCUM_WORD,ACCUM_IN,   /* default - accumulate */

  START_OUT,' ',SKIP_CHAR,START_OUT,  /* toss spaces */
  START_OUT,';',SKIP_CHAR,START_IN,   /* ; switches back to in */
  START_OUT,'>',SKIP_CHAR,START_OUT,  /* consume >'s */
  START_OUT,'\0',RTN_DONE,START_IN,   /* \0 rtns done, arm for restart. */
  START_OUT,'A',START_WORD,ACCUM_OUT, /* default - start output word */

  ACCUM_OUT,' ',RTN_OUT,START_CHAR,   /* blank ends word, expect char next */
  ACCUM_OUT,';',RTN_OUT,START_IN,     /* ; ends out, wrap to in */
  ACCUM_OUT,'>',RTN_OUT,START_OUT,    /* > ends out, expect another out */
  ACCUM_OUT,'\0',RTN_OUT,START_IN,    /* \0 ends word, arm for restart. */
  ACCUM_OUT,'A',ACCUM_WORD,ACCUM_OUT, /* default - gather characters */

  START_CHAR,' ',SKIP_CHAR,START_CHAR,/* skip spaces */
  START_CHAR,';',SKIP_CHAR,START_IN,  /* ; loops back to input */
  START_CHAR,'>',SKIP_CHAR,START_OUT, /* > loops back to out */
  START_CHAR,'\0',RTN_DONE,START_IN,  /* \0 rtns done, rearm */
  START_CHAR,'A',START_WORD,ACCUM_CHAR, /* default - start characterization */

  ACCUM_CHAR,'\\',ACCUM_ESC_WORD,ACCUM_CHAR,  /* escaped character */
  ACCUM_CHAR,';',RTN_CHAR,START_IN,   /* ; ends characterization, exp. in */
  ACCUM_CHAR,'>',RTN_CHAR,START_OUT,  /* > ends char., expect output */
  ACCUM_CHAR,'\0',RTN_CHAR,START_IN,  /* \0 ends char and rearm for input */
  ACCUM_CHAR,'A',ACCUM_WORD,ACCUM_CHAR,   /* accumulate */
  
  LAST_STATE,'A',RTN_DONE,LAST_STATE }; /* last state marker - action 
                                              meaningless */

int get_dep_word(p_word,p_init_string)
char **p_word;    /* return pointer to next word */
char *p_init_string; /* initial dependency description string -
                         used as workspace */

/* if p_init_string is NULL, return next word in previous p_init_string
   in p_word.
   if p_init is not NULL, return first word in p_init_string.
   return an indication of the type of word in p_word from:
        IN_DEP_FILE, OUT_DEP_FILE, CHAR_STRING, or DEP_DONE.

   preconditions: p_init_string is not not NULL, or p_dep_word has been
                  called previously with p_init_string not null.
   postconditions: p_word contains next word delimited by blank,>,;,or \0
                         and return value <> DEP_DONE
                   or p_word is null and return = DEP_DONE.
*/
{
  static char saved_char[2];  /* delimiter of last p_word returned, 
                                 temporarily replaced by \0 in input string. */

  static char *cur_string;        /* last non-null p_init_string */
  static int cur_pos;              /* current scan index in cur_string */

  static int cur_state;            /* record of semantic position in input */
  /* p_init_string <> NULL causes intialization */
  if (p_init_string)
  {
    cur_string=p_init_string;  /* location of string to scan.  */
    cur_pos=0;                 /* start at beginning of new string */
    saved_char[0]=cur_string[cur_pos];  /* negate first saved char restore */
    cur_state=START_IN;        /* expect start of input dependency */
  }

  /* saved_char = \0 indicates previously sent string has been exhausted. */
  if (saved_char[0]!='\0')
  { /* more string to scan.  restore saved character. */
    cur_string[cur_pos]=saved_char[0];

    do
    { /* loop for running automaton.  Find current input in current state */
      /* increment search state until the test character equals the
	 next character in the string, or the next dep_grammar entry to
	 be scanned is associated with a different current state. */
      
      int search_state;
      for (search_state=cur_state;
	   (cur_string[cur_pos]!=dep_grammar[search_state].see[0])&&
	     (dep_grammar[search_state+1].entry_state==cur_state);
	     search_state++);
      
      /* search_state now points to entry which governs activity.
	 set the next state and execute the action. */
      cur_state=dep_grammar[search_state].next_state;
      switch (dep_grammar[search_state].action)
      {
	case START_WORD:
	{ *p_word=(char *) &cur_string[cur_pos]; /* record word address */
	  break;
	}
       
	case RTN_IN:
	{
	  /* save current character and replace with \0, 
	       then return IN_DEP_FILE */
	  saved_char[0]=cur_string[cur_pos];
	  cur_string[cur_pos]='\0';
	  return(IN_DEP_FILE);
	  break;
	}
       
	case RTN_OUT:
	{
	  /* save current character and replace with \0, 
	       then return OUT_DEP_FILE */
	  saved_char[0]=cur_string[cur_pos];
	  cur_string[cur_pos]='\0';
	  return(OUT_DEP_FILE);
	  break;
	}
       
	case RTN_CHAR:
	{
	  /* save current character and replace with \0, 
	       then return CHAR_STRING */
	  saved_char[0]=cur_string[cur_pos];
	  cur_string[cur_pos]='\0';
	  return(CHAR_STRING);
	  break;
	}
	
	case RTN_DONE:
	{
	  *p_word= NULL;
	  return(DEP_DONE);
	  break;
	}

	case ACCUM_WORD:
	case SKIP_CHAR:
	{ /* just increment current position */
	  /* the only diff. between ACCUM and SKIP is whether START was called
	      previously, posting the start pointer. */
	  cur_pos++;
	  break;
	}

	case ACCUM_ESC_WORD:
	{ /* increment position by 2. */
	  cur_pos+= 2;
	  break;
	}
      }    /* end of action switch */
    } while (TRUE);  /* automaton loop */
  } else
  { /* saved character was \0.  no input string */
    return(DEP_DONE);
  }
}

/***************************************************************************/

/* ROUTINES FOR OUTPUTTING STUFF TO THE INTERMEDIATE GRAPH FILE */

char *real_name(p_file)
    char *p_file;
     /*  return the real name of the indicated file given the current
	 directory in my_dir
	 preconditions : my_dir set.  NO ESCAPED /'s IN P_FILE !!!!
	 postconditions : pointer to newly allocated string returned
	                  unless name has inconsistent /../'s
	 */
{
  char *new_name = (char *)malloc(fname_sz*sizeof(char));
  char *cur_char,   /* next character to be scanned for /../ match */
       *prev_slash;  /* pointer to / precedeing a /../ */
  char my_dir[fname_sz];

#if	defined(HPUX)
      getcwd(my_dir, sizeof(my_dir));
#else
      getwd(my_dir);
#endif
  
  /* if first character of name is a /, we're done */
  if(!strncmp(p_file,"/",1)) return(strcpy(new_name,p_file));

  /* if name begins with ./, remove it.
     either way, prepend current directory */
  strcpy(new_name,my_dir);
  strcat(new_name,"/");
  if(!strncmp(p_file,"./",2)) strcat(new_name,p_file+2);
  else strcat(new_name,p_file);

  /* for every /../ imbedded in the file name, remove the
     directory name immediately precedeing it.  */
  for(cur_char=new_name;cur_char[0];cur_char++)
    if(!strncmp(cur_char,"/../",4))
      {
	cur_char[0] = 0;   /* end predeecing string at match */
	prev_slash=strrchr(new_name,'/'); /* find the / preceeding the match */
	if (prev_slash)
	  {
	    prev_slash[0]=0;               /* truncate at precedeing slash */
	    strcat(new_name,cur_char + 3); /* and stick on substr. after /.. */
	    cur_char=new_name;             /* start scan over. */
	  }
	else return(p_file);  /* inconsistent /../'s */
      }
  return(new_name);
}
  
append_cmd(p_stream,p_argc,p_argv)
FILE *p_stream;
int p_argc;
char *p_argv[];
/* place the command consisting of the comcatination of all of the arguements
   in argv from argv[3] on at the end of the file opened in append mode as
   p_stream.  Preceed the command with the keyword STEP.  */
{
  char cmd_string[line_sz];
  int  cur_arg;
  char my_dir[fname_sz];

#ifdef DBGLIB
  printf("append command %s\n",p_argv[4]);
#endif
#if	defined(HPUX)
      getcwd(my_dir, sizeof(my_dir));
#else
      getwd(my_dir);
#endif

  strcpy(cmd_string,"STEP cd ");
  strcat(cmd_string,my_dir);
  strcat(cmd_string,"; ");
  
  for (cur_arg=4;cur_arg<p_argc;++cur_arg)
  { /* add argv[cur_arg] to cmd_string preceeded by a blank.  */
    strcat(cmd_string," ");
    strcat(cmd_string,p_argv[cur_arg]);
  }
  fprintf(p_stream,"%s\n",cmd_string);
}

append_in(p_stream,p_fname,p_size)
FILE *p_stream;
char *p_fname;
int p_size;

/* append the line:
      IN p_fname M p_size
   to the file opened as p_stream.  */
{
#ifdef DBGLIB
  printf("append input  %s\n",p_fname);
#endif
  fprintf(p_stream,"IN %s M %d\n",p_fname,p_size);
}

append_out(p_stream,p_fname,p_size)
FILE *p_stream;
char *p_fname;
int p_size;

/* append the line:
      OUT p_fname M p_size
   to the file opened as p_stream.  */
{
#ifdef DBGLIB
  printf("append output %s\n",p_fname);
#endif
  fprintf(p_stream,"OUT %s M %d\n",p_fname,p_size);
}

append_time(p_stream,p_time)
FILE *p_stream;
int p_time;

/* append the line:
      TIME p_time
   to the file opened as p_stream.  */
{
#ifdef DBGLIB
  printf("append time %d\n",p_time);
#endif
  fprintf(p_stream,"TIME %d\n",p_time);
}

append_end(p_stream)
FILE *p_stream;
/* append the line:
      END
   to the file opened as p_stream.  */
{
#ifdef DBGLIB
  printf("append end\n");
#endif
  fprintf(p_stream,"END\n");
}

append_env (p_stream,p_env)
FILE *p_stream;
char **p_env;
/* append the variables defined in the environment p_env to the end of the
   file opened as p_stream.
   
   p_env is an array of pointers to strings, where a string is an array of
   characters.  The last pointer in *env is null.

*/
{
  char **cur_env;  /* loop index which will point to each environment string */

#ifdef DBGLIB
  printf("append env\n");
#endif
  for (cur_env=environ;*cur_env;++cur_env)
  {
    /* if (strncmp(*cur_env,"TERMCAP=",8)) */
    { fprintf(p_stream,"ENV %s\n",*cur_env);
#ifdef DBGLIB
      printf("%s\n",*cur_env);
#endif
    }
  }
}

int get_file_sz(p_fname)
char *p_fname;

/* get the file size of p_fname if it exists.
   return -1 if fiel doesn't exist.
   use stat to get the information  */

{
  struct stat stat_buf;   /* file status shows up here. */
#ifdef DBGLIB
  printf("getting size of %s\n",p_fname);
#endif

  if  (!stat(p_fname,&stat_buf))
  { /* got it.  just return it. */
    return((int)(stat_buf.st_size));
  }
  else {
#ifdef DBGLIB
    printf("file sz err");
#endif
    return(-1);
  }
}

/****************************************************************************/

/* ROUTINES to evaluate types of commands. */

#define OUTDIV 10       /* expected input size/output size */
#define TIMECON 5000    /* constant time for step execution */
#define TIMEDIV 5       /* expected input size/variable time */

void eval_unk(p_graph_file,p_argc,p_argv,p_cmd_arg)
int p_argc,p_cmd_arg;     /* number of pmksh arguements and the index of the
                             one to be evaluated */
char *p_argv[];             /* pointer to array of arguement strings */
FILE *p_graph_file;       /* stream to post to */

/* post the input, time and output descriptions of an unrecognized command
   preconditions: p_graph_file opened and STEP identifier posted for this
                  command.
   postconditions: input file names and sizes posted.  input file size=-1 if
                     file not in current directory.
                   output file name posted, size=-1.
                   execution time posted as 0.

*/
{
  char *input_word;   /* pointer to word most recently scanned */
  int  input_word_type;  /* type of most recently input word */

  char scratch[line_sz];   /* space for strtok to work */

  char out_file_name[fname_sz]; 

  int input_size=0;         /* accumulate total input size */
  
  /* initialize: nullify output name and make a working copy of the
      dependency string */
  
  strcpy (out_file_name,"");
  strcpy(scratch,p_argv[3]);
  input_word_type=get_dep_word(&input_word,scratch);

  do  /* as long as input lasts */
  { /* respond to the input type received */
    switch (input_word_type)
    {
      case IN_DEP_FILE:
        { int new_in = get_file_sz(input_word);
	  append_in(p_graph_file,real_name(input_word),new_in);
	  input_size += new_in;
          input_word_type=get_dep_word(&input_word,NULL);
          break;}
      
      case OUT_DEP_FILE:
        { /* the char string for unknown commands is the output file
             size and execution time ( if present ) */
          strcpy(out_file_name,input_word);
          
          /* get next word, and process if its CHAR_STRING */
          if((input_word_type=get_dep_word(&input_word,NULL))==CHAR_STRING)
          {
            /* expect 2 fields: outfile size and exec. time.
               if there is only 1 field, use UNIT_TIME for exec. time */
 
            int osize,time;
            if(sscanf(input_word,"%d %d",&osize,&time)>1)
            {
              append_out(p_graph_file,real_name(out_file_name),osize);
              append_time(p_graph_file,time);
            } else {
              append_out(p_graph_file,real_name(out_file_name),osize);
              append_time(p_graph_file,UNIT_TIME);
            }

          } else
          { /* no CHAR_STRING. */
	    if (input_size <= 0)
	      {
		append_out(p_graph_file,real_name(out_file_name),0);
		append_time(p_graph_file,UNIT_TIME);
	      }
	    else
	      {  /* make a wild guess at output size and execution time */
		append_out(p_graph_file,real_name(out_file_name),
			   input_size / OUTDIV);
		append_time(p_graph_file,TIMECON + (input_size / TIMEDIV));
	      }
          }
          break;}

      case DEP_DONE:
        { /* done posting dependencies */
          return;
           break;}
    } /* end of word type switch */
  } while (TRUE); /* loop for words in input */
}


/*  LINEAR SUFFIX DEPENDENCY EVALUATOR  

This routine accepts a dependency string and an array describing a 
method of evaluating dependencies based on the suffixes of the files
named.  The rule is that each file with a given suffix contributes
output file size and execution time linearly proportional to that file's size.
Furthermore the directories to be searched for that file may depend on the
suffix.

Output to the graph file is controlled by an array in which each element
has the following type:  */

#define SUFF_SZ 10
typedef struct {
                 char suffix[SUFF_SZ];  /* suffix which will match */
                 char dir[fname_sz];    /* directory to check for match */
                 float out_coeff,       /* ratio of input sz to output
                                            contribution for this suffix */
                       time_coeff;      /* ratio of input size to ms of
                                            contribution to exec. time */
               } lin_suff_type;

/* for each input file the suffix map is scanned until a suffix matches AND
   the input file is found in the associated directory.  The first suffix
   and directory to succeed wins so order may make a difference.  List
   suffixes in decreasing order of length.
   
   The null string suffix matches and file name.
   directories are literally prepended to the file name with an
       interveneing /.
   The null directory checks the file name without modification.
*/

bool is_suff(p_word,p_suffix)
char *p_word,*p_suffix;
/* return TRUE if p_suffix is a suffix of p_word,
   FALSE otherwise.  */
{
  int word_pos,suffix_pos; /* current position in word and suffix 
                              respectively. */

  /* start at the end */
  word_pos=strlen(p_word)-1;

  /* fail if any characters mismatch or word ends before suffix. */
  for(suffix_pos=strlen(p_suffix)-1;
      suffix_pos>=0;suffix_pos--)
  {
    if ((word_pos<0)||
        (p_word[word_pos]!=p_suffix[suffix_pos])) return(FALSE);
    word_pos--;
  }
  return(TRUE);
}

lin_suff_dep(p_graph_file,p_dep_string,p_suff_map,p_num_suffs,p_con_time)
FILE *p_graph_file;  /* stream to output graph file */
char *p_dep_string;  /* dependency string */
lin_suff_type p_suff_map[];  /* map of suffixes to try */
int p_num_suffs;     /* number of elements in the map. */
int p_con_time;      /* constant time for starting application */

/* post the input and output files and sizes, and the execution time
   based on p_suff_map.

   preconditions:  p_graph_file is opened for append and the step
                   being evaluated has been posted.
                   p_suff_map contains p_num_suffs elements
   postconditions: IN, OUT, and TIME lines describing the step have
                   been posted.
*/
{
  char *input_word;   /* pointer to word most recently scanned */
  int  input_word_type;  /* type of most recently input word */

  char scratch[line_sz];   /* space for get_dep_word to work */

  bool init_outsz= TRUE;   /* TRUE if time and out accumulators should be
                               reinitialized when input comes around again */

  float outsz,             /* output file size accumulator */
        exec_time=0;       /* execution time accumulator */

  /* initialize: nullify output name and make a working copy of the
      dependency string */
  
  strcpy(scratch,p_dep_string);
  input_word_type=get_dep_word(&input_word,scratch);

  do  /* as long as input lasts */
  { /* respond to the input type received */
    switch (input_word_type)
    {
      case IN_DEP_FILE:
        { /* look for the file */
          int infile_sz= (-1), /* size of file found - initially not found */
              cur_suffix;   /* suffix being tried */
          char fname[fname_sz]; /* file name construction space */

          /* initialize exec_time and outsz if necessary */
          if (init_outsz)
          { 
            init_outsz= FALSE;
            outsz=0;
          }

          /* scan suffixes until file found or out of suffixes */
          for(cur_suffix=0;(cur_suffix<p_num_suffs)&&
                           (infile_sz==-1);cur_suffix++)
           if (is_suff(input_word,p_suff_map[cur_suffix].suffix))
            { if (p_suff_map[cur_suffix].dir[0]=='\0')
                infile_sz=get_file_sz(strcpy(fname,input_word));
              else
              { /* tack the directory onto the input word */
                
                strcpy(fname,p_suff_map[cur_suffix].dir);
                strcat(fname,"/");
                infile_sz=get_file_sz(strcat(fname,input_word));
              }
            }
          /* now infile_sz is either the size of the file if found,
             or -1 if not.  If file found, fname contains the full name. */
          if (infile_sz==-1) append_in(p_graph_file,real_name(input_word),-1);
          else
          { /* post the actual input file name found and accumulate 
               output size and time figures */
            cur_suffix--;   /* suffix incremented once too far in this case */
            append_in(p_graph_file,real_name(fname),infile_sz);
            exec_time+=infile_sz*p_suff_map[cur_suffix].time_coeff;
            outsz+=infile_sz*p_suff_map[cur_suffix].out_coeff;
          }

          input_word_type=get_dep_word(&input_word,NULL);
          break;}
      
      case OUT_DEP_FILE:
        { /* the char string for unknown commands is the output file
             size and execution time ( if present ) */
          append_out(p_graph_file,real_name(input_word),(int)outsz);
          init_outsz= TRUE;
          input_word_type=get_dep_word(&input_word,NULL);
          break;}
  
      case CHAR_STRING:
        {
           input_word_type=get_dep_word(&input_word,NULL);
           break;
        }
      case DEP_DONE:
        { /* done posting dependencies */
          append_time(p_graph_file,(int)exec_time + p_con_time);
          return;
          break;}
    } /* end of word type switch */
  } while (TRUE);/* loop for words in input */
}

/*  Suffix map for cc  */
#define CC_SUFFIX_COUNT 5
#define C_SRC_OUT .3
#define C_SRC_TIME 1
#define C_OBJ_TIME .1
#define C_INCL_TIME .05
#define C_LIB_TIME .1
lin_suff_type cc_suffix[CC_SUFFIX_COUNT]={
  ".c","",C_SRC_OUT,C_SRC_TIME,    /* normal source files */
  ".o","",1.0,C_OBJ_TIME,          /* linked object files */
  ".h","",0,C_INCL_TIME,            /* include files */
  ".h","/usr/include",0,C_INCL_TIME,
  ".a","",1.0,C_LIB_TIME           /* library files */
  };

void eval_cc(p_graph_file,p_argc,p_argv,p_cmd_arg)
FILE *p_graph_file;     /* destination of new lines */
char *p_argv[];         /* run string arguements */
int p_argc,p_cmd_arg;   /* total number of args and index of recognised cmd. */

/* evaluate the dependency file sizes and estimated execution time of the
   indicated cc command.  Post lins describing these to p_graph_file 

   preconditions: p_graph_file is opened for append, and STEP has been posted
                  for current command.
   postconditions: IN, OUT, and TIME entries posted for command.
*/

{
  /* simply use linear suffix method for now */
  lin_suff_dep(p_graph_file,p_argv[3],cc_suffix,CC_SUFFIX_COUNT,5000);
}

/***************************************************************************/

/* ROUTINES TO MANIPULATE THE LIBRARY STRUCTURE */
/* each entry in the cmd_lib associates a command with an evaluation
    routine.  The first command in the list is used when an unrecognised
    command is encountered.  The null command is used if there is no command
*/

typedef struct { char cmd_name[fname_sz];  /* a command recognized */
                 void (*eval_cmd) ();        /* pointer to routine used to
                                              evaluate the command.  */
               } cmd_lib_node;

list_type cmd_lib;     /* the command library directory */

add_to_lib(p_cname,p_eval_ptr)
char p_cname[fname_sz];         /* name of command to be added */
void (*p_eval_ptr) ();            /* pointer to procedure to evaluate command */

/* add a command characterization procedure to the library directory */
/* preconditions : cmd_lib has been initialized
                   p_eval_ptr is the library routine which accepts standard
                       pmklib parameters and evaluates the named command.

   postconditions : pmksh will call *p_eval_ptr to evaluate the named command

*/
{
  /* allocate the new node and the pointer to it */
  cmd_lib_node *new_node=(cmd_lib_node *)malloc(sizeof(cmd_lib_node));

  /* initialize fields in the new node and insert in library. */
  strcpy(new_node->cmd_name,p_cname);
  new_node->eval_cmd=p_eval_ptr;
  enqueue(new_node,&cmd_lib);
}

init_lib()

/* initialize the command recognition library */
/* preconditions: none
   postconditions: all of the commands listed here are in the library
                   directory
*/
{
  new_list(&cmd_lib);    /* begin with an enpty list */
  add_to_lib("unknown",eval_unk);   /* evaluate unknown command */
  add_to_lib("cc",eval_cc);         /* evaluate compilation */

}

eval_lib_cmd(p_graph_file,p_argc,p_argv,p_cmd_arg)
FILE *p_graph_file;    /* file to which description will be posted */
char *p_argv[];        /* run string arguments */
int p_argc,p_cmd_arg;  /* number of args and index of cmd. to eval */

/* execute the command evaluation routine referenced by p_argv[p_cmd_arg]
   in cmd_lib 
   preconditions: cmd_lib must be initialized,
                  and first element must reference unknown command
                  eval routine.  
                  also see preconditions of command evaluation routines
   postconditions: command eval. routine executed 
*/
{
  char cmd_to_find[fname_sz];   /* name of command to find */
  list_node *cur_node;          /* list node in cmd_lib being tested */

  if (p_cmd_arg<p_argc)
    strcpy(cmd_to_find,p_argv[p_cmd_arg]);
  else
    strcpy(cmd_to_find,"");   /* if command after last arg. use "" to indicate
                                   no command at all. */

  for(cur_node=cmd_lib.head;cur_node;cur_node=(list_node *)(cur_node->next))
  {
    if (!strcmp(cmd_to_find,((cmd_lib_node *)cur_node->content)->cmd_name))
    { /* command found   execute associated evaluation routine */
      (*(((cmd_lib_node *) cur_node->content)->eval_cmd))
       (p_graph_file,p_argc,p_argv,p_cmd_arg);

      return;
    }
  }
  /* no command found.  use first entry as unknown */
  (*(((cmd_lib_node *) cmd_lib.head->content)->eval_cmd))
   (p_graph_file,p_argc,p_argv,p_cmd_arg);
}


