/*  $RCSfile: teller.c,v $ $Revision: 2.9 $ $Date: 90/08/02 13:28:15 $  */
#include "isis.h"
#include "bank.h"
#include <string.h>
#include <ctype.h>

int port_nr = 0;            /* ISIS port number */

address *server;
bool connected;
bool test = FALSE;

bool connect_to_bank();
int _filbuf();


void
help()
{
    print("\
<rpt> d(eposit) <account> <amount>                    \n\
<rpt> w(ithdraw) <account> <amount>                   \n\
<rpt> t(ransfer) <from-account> <to-account> <amount> \n\
<rpt> i(nquire) <account>                             \n\
quit                                                  \n\
test                                                  \n\
h(elp)                                                \n\
                                                      \n\
<rpt> denotes the number of repetitions of the command and is optional \n\
Accounts are specified by alphabetic names.           \n\
Accounts are created on demand with zero balance.     \n\
Amounts are whole numbers of dollars.                 \n");
}

void
bank_error(code)
  int code;
  /* Display an application level error message. */
{
    switch (code) {
      case 0:
        return;
      case transaction_begin_contention:
        print("Transaction begin contention, try again\n");
        return;
      case bank_account_overdrawn:
        print("Account would be overdrawn\n");
        return;
      case bank_account_conflict:
        print("Concurrency conflict with another transaction\n");
        return;
      case bank_service_unavailable:
        print("Cannot contact bank service\n");
        return;
      default:
        print("Unknown bank service error code: %d\n", code);
        return;
    }
}
        
void
test_point(s)
  char *s;
  /* Allow the user to force control particular event sequences, or failure
     modes in multiple processes. */
{
    if (test) {
        print("*** %s *** ", s);
        getchar();
    }
}

int
start()
  /* Begin the transaction, with optional display of the transaction id. */
{
    if (x_begin() < 0) {
        if (isis_errno == IE_AGAIN) {
            return(transaction_begin_contention);
        } else {
            isis_perror("x_begin");
            exit(1);
        }
    }
    if (test) {
        print("--- transaction ");
        paddr(x_getid());
        print(" ---\n");
    }
    return(0);
}

bool
complete(success)
  int success;
  /* Commit or abort the transaction, returning TRUE if the commit was successful,
     and FALSE if the transaction aborted.
     Optionally allow the user to do an abort. */
{
    bool commit;

    if (success == 0) {
        commit = TRUE;
    } else {
        bank_error(success);
        commit = FALSE;
    }
        
    /* In test mode we ask the user whether to commit. */
    if (test) {
        print("*** commit? (y/n)[y] *** ");
        if (getchar() == 'n') {
            print(" aborting\n");
            commit = FALSE;
        } else {
            print(" committing\n");
            commit = TRUE;
        }
    }

    /* Actually do the commit or abort. */
    if (commit) {
        if (x_commit(2) == 0) {
            return(TRUE);
        } else {
            if (isis_errno == IE_ABORT) {
                print("Transaction aborted\n");
                return(FALSE);
            } else { 
                isis_perror("x_commit");
                exit(1);
            }
        }
    } else {
        if (x_abort() == 0) {
            return(FALSE);
        } else {
            isis_perror("x_abort");
            exit(1);
        }
    }
    return(FALSE); /* Keep compiler happy. */
}

/*******************************************************************************
 Routines to do bank operations at the bank server. 
*******************************************************************************/

bool
deposit(repeat, name, amount)
  int repeat;
  char *name;
  int amount;
{
    int success = 0;

    while ((repeat--) > 0 && success == 0) {
        success = start();
        if (success != 0) {
            bank_error(success);
            return(FALSE);
        }
        test_point("about to deposit");
        if (cbcast(server, BANK_DEPOSIT,
                   "%s %d", name, amount, 1, "%d", &success) != 1)
        {
            success = bank_service_unavailable;
            connected = FALSE;
        }
        complete(success);
    }
    return(success == 0);
}

bool
withdraw(repeat, name, amount)
  int repeat;
  char *name;
  int amount;
{
    int success = 0;
    
    while ((repeat--) > 0 && success == 0) {
        int retries = 0;
        while (TRUE) {
            success = start();
            if (success != 0) {
                bank_error(success);
                return(FALSE);
            }
            test_point("about to withdraw");
            if (cbcast(server, BANK_WITHDRAW,
                       "%s %d", name, amount, 1, "%d", &success) == 1)
            {
                if (success == bank_account_conflict &&
                    (++retries) < 3)
                {
                    bank_error(success);
                    print("aborting and retrying\n");
                    x_abort();
                    continue;
                }
            } else {
                success = bank_service_unavailable;
                connected = FALSE;
            }
            break;
        }
        complete(success);
    }
    return(success == 0);
}

bool
transfer(repeat, name1, name2, amount)
  int repeat;
  char *name1, *name2;
  int amount;
{
    int success = 0;

    while ((repeat--) > 0 && success == 0) {
        int retries = 0;
        while (TRUE) {
            success = start();
            if (success != 0) {
                bank_error(success);
                return(FALSE);
            }
            
            /* Withdraw from name1. */
            test_point("about to withdraw");
            if (cbcast(server, BANK_WITHDRAW,
                       "%s %d", name1, amount, 1, "%d", &success) == 1)
            {
                if (success == bank_account_conflict &&
                    (++retries) < 3)
                {
                    bank_error(success);
                    print("aborting and retrying\n");
                    x_abort();
                    continue;
                }
            } else {
                success = bank_service_unavailable;
                connected = FALSE;
            }
            break;
        }

        if (success == 0) {
            /* Deposit into name2. */
            test_point("about to deposit");
            if (cbcast(server, BANK_DEPOSIT,
                       "%s %d", name2, amount, 1, "%d", &success) != 1)
            {
                success = bank_service_unavailable;
                connected = FALSE;
            }
        }
        complete(success);
    }
    return(success == 0);
}

bool
inquire(repeat, name, balance, cleared)
  int repeat;
  char *name;
  int *balance, *cleared;
{
    int success = 0;

    while ((repeat--) > 0 && success == 0) {
        success = start();
        if (success != 0) {
            bank_error(success);
            return(FALSE);
        }
        if (cbcast(server, BANK_INQUIRY,
                   "%s", name, 1, "%d %d", balance, cleared) != 1)
        {
            success = bank_service_unavailable;
            connected = FALSE;
        } else {
            if (*balance < 0) { /* balance is actually an error code in this case. */
                success = *balance; 
            }
        }
        complete(success);
        if (success == 0) {
            if (*cleared == *balance) {
                print("Balance $%d\n", *balance);
            } else {
                print("Balance $%d, of which $%d is cleared\n", *balance, *cleared);
            }
        }
    }
    return(success == 0);
}

/*******************************************************************************
 Input parsing routines. 
*******************************************************************************/

bool
number(s)
  char *s;
  /* Return TRUE iff s consists of digits. */
{
    if (s == NULL) {
        return(FALSE);
    };
    while (*s != '\000') {
        if (!isdigit(*s)) {
            return(FALSE);
        }
        s++;
    }
    return(TRUE);
}

bool
alpha(s)
  char *s;
  /* Return TRUE iff s consists of alphabetic characters. */
{
    if (s == NULL) {
        return(FALSE);
    };
    while (*s != '\000') {
        if (!isalpha(*s)) {
            return(FALSE);
        }
        s++;
    }
    return(TRUE);
}

#define delim " \t,\n"
   /* Delimiters between tokens on input lines. */

bool
get_name(name, what)
  char **name;
  char *what;
  /* Obtain the next token using strtok, and parse it as a alphabetic name
     into 'name'. */
{
    *name = strtok(NULL, delim);
    if (*name == NULL ||
        !alpha(*name))
    {
        print("%s expected\n", what);
        return(FALSE);
    }
    return(TRUE);
}

bool
get_number(num, what)
  int *num;
  char *what;
  /* Obtain the next token using strtok, and parse it as an integer
     into 'num'. */
{
    char *name;
    name = strtok(NULL, delim);
    if (name == NULL ||
        !number(name))
    {
        print("%s expected\n", what);
        return(FALSE);
    }
    *num = atoi(name);
    return(TRUE);
}

bool
com_equal(s, command,min)
  char *command;
  char *s;
  int min;
  /* Return TRUE iff s is a prefix of command, of length at least min. */
{
    int n = strlen(s);
    if (n < min) {
        return(FALSE);
    } else {
        return(strncmp(command, s, n) == 0);
    }
}
  

bool
command()
  /* Read one line from input and execute it as a command.
     Return FALSE when there is no more input, or to quit. */
{
    char line[255];
    char *arg;
    int repeat;
    char *acc, *acc2;
    int amount, balance, cleared;
    condition has_input = 0;
    
    print(" > ");
    begin
    {
	int imask = 1<<fileno(stdin);
	isis_select(32, &imask, 0, 0, 0);
    }
    if(fgets(line, 255, stdin) == NULL)
        return(FALSE);

    if (!connected) { /* After waiting for user input, try to connect again. */
        connect_to_bank();
    }
        
    arg = strtok(line, delim);
    if (!arg) {
        return(TRUE);
    }

    if (number(arg)) {
        repeat = atoi(arg);        
        arg = strtok(NULL, delim);
        test = FALSE;  /* Don't want to be interactive when repeating. */
    } else {
        repeat = 1;
    }
     
    if (!alpha(arg)) {
        if (arg != NULL) {
            print("command or repeat count expected: %s \n", arg);
        }
        return(TRUE);
    }
        
    if (com_equal(arg, "deposit", 1)) {
        if (get_name(&acc, "account name") && get_number(&amount, "amount")) {
            deposit(repeat, acc, amount);
        }
    } else if (com_equal(arg, "withdraw", 1)) {
        if (get_name(&acc, "account name") && get_number(&amount, "amount")) {
            withdraw(repeat, acc, amount);
        }
    } else if (com_equal(arg, "transfer", 1)) {
        if (get_name(&acc, "FROM account name") &&
            get_name(&acc2, "TO account name") &&
            get_number(&amount, "amount")) {
            transfer(repeat, acc, acc2, amount);
        }
    } else if (com_equal(arg, "inquire", 1)) {
        if (get_name(&acc, "account name")) {
            inquire(repeat, acc, &balance, &cleared);
        }
    } else if (com_equal(arg, "test", 4)) {
        test = !test;
        if (test) {
            print("Testing\n");
        } else {
            print("Testing off\n");
        }
    } else if (com_equal(arg, "help", 1) ||
               com_equal(arg, "?", 1))
    {
        help();
    } else if (com_equal(arg, "quit", 4)) {
        return(FALSE);
    } else {
        print("command expected: %s, type 'help' for help \n", arg);        
    }

    if (arg = strtok(NULL, delim)) {
        print("extraneous ignored:");
        do {
            print(" %s", arg);
        } while (arg = strtok(NULL, delim));
        print("\n");
    }
    return(TRUE);
}

bool
connect_to_bank()
  /* (Re)connect to the bank service. */
{
    if (connected) {
        pg_leave(server);
        connected = FALSE;
    }
    server = pg_lookup(BANK_SERVICE);
    if (addr_isnull(server)) {
        print("Teller unable to contact bank service\n");
        return(FALSE);
    }
    if (pg_client(server, "") < 0) {
        isis_perror("Teller unable to do pg_client");
        return(FALSE);
    }
    connected = TRUE;
    return(TRUE);
}

void
teller_control()
{
    isis_start_done();
    if (!connect_to_bank()) {
        exit(1);
    }

    while (command()) {
    };

    exit(0);
}

void
main(argc, argv)
  int   argc;
  char  **argv;
{
    print("Bank teller\n");
    /* Read in command line arguments:
       <port-nr>           Isis port number (optional).
    */
   
while(argc-- > 1) {
        ++argv;
        switch(**argv) {
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            port_nr= atoi(*argv);
            continue;
          badarg:
            panic("Bad argument: <%s>\n", *argv);
        }
    }

/* Initialize and run ISIS */
    isis_init(port_nr);
    isis_mainloop(teller_control, NULLARG);
}
