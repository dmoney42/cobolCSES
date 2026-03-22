       FD  PARENT-MASTER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 89 CHARACTERS
           DATA RECORD IS PARENT-MASTER-RECORD.

       01  PARENT-MASTER-RECORD.
           05  PM-PARENT-ID          PIC 9(8).
           05  PM-PARENT-NAME        PIC X(20).
           05  PM-ADDRESS            PIC X(30).
           05  PM-MONTHLY-OBLIG      PIC 9(4)V99.
           05  PM-CASE-STATUS        PIC X(1).
           05  PM-TOTAL-ARREARS      PIC 9(7)V99.
           05 PM-STATUS-FLAG        PIC X(15).
