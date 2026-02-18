       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHILD-SUPPORT-ES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARENT-MASTER-FILE
               ASSIGN TO "data/parent_master.dat".

           SELECT CHILD-MASTER-FILE
               ASSIGN TO "data/child_master.dat".

           SELECT PAYMENT-TRANSACTION-FILE
               ASSIGN TO "data/payment_transactions.dat".

           SELECT UPDATED-PARENT-FILE
               ASSIGN TO "data/parent_master_updated.dat".
           
       DATA DIVISION.
       FILE SECTION.

             COPY "PARENT-MASTER".
             COPY "CHILD-MASTER".
             COPY "PAYMENT-TRANSACTIONS".

       FD  UPDATED-PARENT-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 74 CHARACTERS
           DATA RECORD IS UPDATED-PARENT-RECORD.

       01  UPDATED-PARENT-RECORD.
           05  UP-PARENT-ID          PIC 9(8).
           05  UP-PARENT-NAME        PIC X(20).
           05  UP-ADDRESS            PIC X(30).
           05  UP-MONTHLY-OBLIG      PIC 9(4)V99.
           05  UP-CASE-STATUS        PIC X(1).
           05  UP-TOTAL-ARREARS      PIC 9(7)V99.           
       
       WORKING-STORAGE SECTION.

       01  WS-EOF-PAYMENTS        PIC X VALUE "N".
       01  WS-EOF-PARENTS         PIC X VALUE "N".
       01  WS-EOF-CHILDREN        PIC X VALUE "N".

       01  WS-PAYMENT-AMOUNT-NUM  PIC 9(6)V99.

       01  WS-PARENT-FOUND        PIC X VALUE "N".


       PROCEDURE DIVISION.
           DISPLAY "CHILD SUPPORT ENFORCEMENT SYSTEM STARTED".

           OPEN INPUT
                PARENT-MASTER-FILE
                CHILD-MASTER-FILE
                PAYMENT-TRANSACTION-FILE.
           
           OPEN OUTPUT UPDATED-PARENT-FILE.

           DISPLAY "FILES OPENED SUCCESSFULLY".

           READ PARENT-MASTER-FILE
               AT END MOVE "Y" TO WS-EOF-PARENTS
           END-READ

           READ CHILD-MASTER-FILE
               AT END MOVE "Y" TO WS-EOF-CHILDREN
           END-READ

           READ PAYMENT-TRANSACTION-FILE
               AT END MOVE "Y" TO WS-EOF-PAYMENTS
           END-READ

           DISPLAY "We read all files successfully.".


       *> ============================
       *> MAIN PAYMENT PROCESSING LOOP
       *> ============================

           PERFORM UNTIL WS-EOF-PAYMENTS = "Y"

               DISPLAY "PROCESSING PAYMENT RECORD"

               MOVE PT-PAYMENT-AMOUNT TO WS-PAYMENT-AMOUNT-NUM

               MOVE "N" TO WS-PARENT-FOUND
               MOVE "N" TO WS-EOF-PARENTS

               CLOSE PARENT-MASTER-FILE
               OPEN INPUT PARENT-MASTER-FILE

               READ PARENT-MASTER-FILE
                   AT END MOVE "Y" TO WS-EOF-PARENTS
               END-READ

               PERFORM UNTIL WS-EOF-PARENTS = "Y"
                          OR WS-PARENT-FOUND = "Y"

                   IF PT-PARENT-ID = PM-PARENT-ID
                       MOVE "Y" TO WS-PARENT-FOUND
                   ELSE
                       READ PARENT-MASTER-FILE
                           AT END MOVE "Y" TO WS-EOF-PARENTS
                       END-READ
                   END-IF

               END-PERFORM

               IF WS-PARENT-FOUND = "Y"
                  SUBTRACT WS-PAYMENT-AMOUNT-NUM
                      FROM PM-TOTAL-ARREARS
              
                  DISPLAY "UPDATED ARREARS FOR PARENT: " PM-PARENT-ID
                  DISPLAY "NEW ARREARS BALANCE: " PM-TOTAL-ARREARS
               ELSE
                  DISPLAY "PARENT NOT FOUND FOR PAYMENT: " PT-PARENT-ID
               END-IF

               READ PAYMENT-TRANSACTION-FILE
                   AT END MOVE "Y" TO WS-EOF-PAYMENTS
               END-READ

           END-PERFORM.


           CLOSE PARENT-MASTER-FILE
                 CHILD-MASTER-FILE
                 PAYMENT-TRANSACTION-FILE.

           STOP RUN.
