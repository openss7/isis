/*  $RCSfile: bank.h,v $ $Revision: 2.9 $ $Date: 90/08/02 13:27:55 $  */
#define BANK_DEPOSIT 1
#define BANK_WITHDRAW 2
#define BANK_INQUIRY 3

#define BANK_SERVICE "bank-service"

#define bank_account_overdrawn -1
#define bank_account_conflict  -2
#define bank_service_unavailable -3
#define bank_bad_argument -4
#define transaction_begin_contention -5

#define max_account_name 25
