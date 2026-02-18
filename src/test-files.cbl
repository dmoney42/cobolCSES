       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-FILES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARENT-MASTER-FILE
               ASSIGN TO "data/parent_master.dat".
           SELECT CHILD-MASTER-FILE
               ASSIGN TO "data/child_master.dat".
           SELECT PAYMENT-TRANSACTION-FILE
               ASSIGN TO "data/payment_transactions.dat".


       DATA DIVISION.
       FILE SECTION.
           COPY "PARENT-MASTER".
           COPY "CHILD-MASTER".
           COPY "PAYMENT-TRANSACTIONS".

       WORKING-STORAGE SECTION.
       01  WS-DUMMY PIC X.

       PROCEDURE DIVISION.
           DISPLAY "COPYBOOKS LOADED SUCCESSFULLY".
           STOP RUN.
