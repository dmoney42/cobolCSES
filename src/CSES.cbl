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

       01  WS-REPORT-COUNTERS.
           05  WS-TOTAL-PARENTS            PIC 9(7)     VALUE 0.
           05  WS-CURRENT-COUNT            PIC 9(7)     VALUE 0.
           05  WS-DELINQUENT-COUNT         PIC 9(7)     VALUE 0.
           05  WS-SEVERE-COUNT             PIC 9(7)     VALUE 0.
           05  WS-TOTAL-ARREARS-SUM        PIC 9(9)V99  VALUE 0.



       PROCEDURE DIVISION.
           DISPLAY "CHILD SUPPORT ENFORCEMENT SYSTEM STARTED".

           OPEN INPUT
                PARENT-MASTER-FILE
                CHILD-MASTER-FILE
                PAYMENT-TRANSACTION-FILE.
           
           OPEN OUTPUT UPDATED-PARENT-FILE.

           DISPLAY "FILES OPENED SUCCESSFULLY".

           MOVE 0 TO WS-TOTAL-PARENTS
                     WS-CURRENT-COUNT
                     WS-DELINQUENT-COUNT
                     WS-SEVERE-COUNT
                     WS-TOTAL-ARREARS-SUM.
            

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
       *> ======================================
       *> SECOND PASS LOOP TO UPDATE ALL RECORDS
       *> ======================================
               IF WS-PARENT-FOUND = "Y"
                  SUBTRACT WS-PAYMENT-AMOUNT-NUM
                  FROM PM-TOTAL-ARREARS
                  MOVE PARENT-MASTER-RECORD 
                  TO UPDATED-PARENT-RECORD
       
       *>         Counter updates for reporting
                  ADD 1 TO WS-TOTAL-PARENTS

                  WRITE UPDATED-PARENT-RECORD              

                  DISPLAY "UPDATED ARREARS FOR PARENT: " PM-PARENT-ID
                  DISPLAY "NEW ARREARS BALANCE: " PM-TOTAL-ARREARS
               ELSE
                  DISPLAY "PARENT NOT FOUND FOR PAYMENT: " PT-PARENT-ID
               END-IF

               READ PAYMENT-TRANSACTION-FILE
                   AT END MOVE "Y" TO WS-EOF-PAYMENTS
               END-READ

           END-PERFORM.

           MOVE "N" TO WS-EOF-PARENTS.
           CLOSE PARENT-MASTER-FILE.
           OPEN INPUT PARENT-MASTER-FILE.
           
           READ PARENT-MASTER-FILE
               AT END MOVE "Y" TO WS-EOF-PARENTS
           END-READ
           
           PERFORM UNTIL WS-EOF-PARENTS = "Y"
              MOVE PARENT-MASTER-RECORD TO UPDATED-PARENT-RECORD
              ADD 1 TO WS-TOTAL-PARENTS
              MOVE "CURRENT" TO PM-STATUS-FLAG

              IF PM-TOTAL-ARREARS > 500
                  MOVE "SEVERELY DELINQUENT" TO PM-STATUS-FLAG
              ELSE
                  IF PM-TOTAL-ARREARS > 0
                      MOVE "DELINQUENT" TO PM-STATUS-FLAG
                  END-IF
              END-IF

                 ADD PM-TOTAL-ARREARS TO WS-TOTAL-ARREARS-SUM

                  *> Then increment the correct status counter each 
                  *> status
                  IF PM-STATUS-FLAG = "CURRENT"
                      ADD 1 TO WS-CURRENT-COUNT
                  ELSE
                      IF PM-STATUS-FLAG = "DELINQUENT"
                          ADD 1 TO WS-DELINQUENT-COUNT
                      ELSE
                          IF PM-STATUS-FLAG = "SEVERELY DELINQUENT"
                              ADD 1 TO WS-SEVERE-COUNT
                          END-IF
                      END-IF
                  END-IF

                  WRITE UPDATED-PARENT-RECORD

              READ PARENT-MASTER-FILE
                  AT END MOVE "Y" TO WS-EOF-PARENTS
              END-READ

           END-PERFORM.


           CLOSE PARENT-MASTER-FILE
                 CHILD-MASTER-FILE
                 PAYMENT-TRANSACTION-FILE
                 UPDATED-PARENT-FILE.

           STOP RUN.
