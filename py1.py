Title: Customer Account Update Batch - Modern COBOL Implementation

Overview:
This program updates customer account balances using daily transaction records and produces a financial audit report. The legacy COBOL code has been refactored to follow modern COBOL standards: modular paragraph structure, meaningful variable names, structured control flow (PERFORM/EVALUATE), improved error handling, and thorough inline documentation. Key improvements include separation of business logic and reporting, elimination of GO TO statements, and enhanced maintainability for onboarding and future development.

Refactored COBOL Code:

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTUPDT-REFACTORED.

       AUTHOR. Senior Legacy Systems Documentation Specialist.

       *--- Environment setup and file assignments
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-MASTER-IN   ASSIGN TO 'ACCTMAST.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCT-MASTER-OUT  ASSIGN TO 'ACCTMAST.NEW'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCT-TRAN-IN     ASSIGN TO 'ACCTTRAN.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT AUDIT-REPORT-OUT ASSIGN TO 'ACCTREP.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       *--- Account Master Input Record
       FD  ACCT-MASTER-IN.
       01  ACCT-MASTER-REC.
           05  M-ACCT-NUMBER         PIC X(10).
           05  M-ACCT-NAME           PIC X(30).
           05  M-ACCT-BALANCE        PIC S9(9)V99 COMP-3.
           05  M-LAST-ACTIVITY-DATE  PIC 9(8).
           05  FILLER                PIC X(12).

       *--- Account Master Output Record
       FD  ACCT-MASTER-OUT.
       01  ACCT-MASTER-OUT-REC      PIC X(60).

       *--- Transaction Input Record
       FD  ACCT-TRAN-IN.
       01  ACCT-TRAN-REC.
           05  T-ACCT-NUMBER         PIC X(10).
           05  T-TRAN-CODE           PIC X(2).
           05  T-TRAN-AMOUNT         PIC S9(7)V99 COMP-3.

       *--- Audit Report Output Record
       FD  AUDIT-REPORT-OUT.
       01  AUDIT-REPORT-REC         PIC X(80).

       WORKING-STORAGE SECTION.
       *--- Run date and end-of-file flags
       77  WS-RUN-DATE              PIC 9(8) VALUE ZEROS.
       77  WS-EOF-MASTER            PIC X VALUE 'N'.
       77  WS-EOF-TRAN              PIC X VALUE 'N'.

       *--- Counters for reporting
       77  WS-ACCTS-PROCESSED       PIC 9(6) VALUE ZEROS.
       77  WS-ACCTS-UPDATED         PIC 9(6) VALUE ZEROS.
       77  WS-ACCTS-NO-TRANS        PIC 9(6) VALUE ZEROS.
       77  WS-UNKNOWN-TRAN-CNT      PIC 9(6) VALUE ZEROS.

       *--- Flags for transaction processing
       01  WS-TRANSACTION-FOUND     PIC X VALUE 'N'.
       01  WS-TRANSACTION-APPLIED   PIC X VALUE 'N'.

       *--- Report line buffer
       01  WS-REPORT-LINE.
           05  WS-RPT-TEXT          PIC X(80).

       *--- Transaction search variables
       01  WS-TRAN-SEARCH-KEY       PIC X(10).
       01  WS-TRAN-ACCT-NUMBER      PIC X(10).
       01  WS-TRAN-CODE             PIC X(2).
       01  WS-TRAN-AMOUNT           PIC S9(7)V99 COMP-3.

       *--- Master output record buffer
       01  WS-MASTER-OUT-REC.
           05  OUT-ACCT-NUMBER      PIC X(10).
           05  OUT-ACCT-NAME        PIC X(30).
           05  OUT-ACCT-BALANCE     PIC S9(9)V99 COMP-3.
           05  OUT-LAST-ACTIVITY    PIC 9(8).
           05  FILLER               PIC X(12).

       *--- Summary totals for report
       01  WS-REPORT-TOTALS.
           05  WS-TOTAL-ACCTS       PIC 9(6) VALUE ZEROS.
           05  WS-TOTAL-UPDATED     PIC 9(6) VALUE ZEROS.
           05  WS-TOTAL-NO-TRANS    PIC 9(6) VALUE ZEROS.
           05  WS-TOTAL-UNKNOWN     PIC 9(6) VALUE ZEROS.

       *--- Detail line for audit report
       01  WS-DETAIL-LINE.
           05  DL-ACCT-NUMBER       PIC X(10).
           05  DL-ACCT-NAME         PIC X(30).
           05  DL-TRAN-CODE         PIC X(2).
           05  DL-TRAN-AMOUNT       PIC $$,$$$,$$9.99-.
           05  DL-NEW-BALANCE       PIC $$,$$$,$$9.99-.
           05  DL-STATUS            PIC X(20).

       PROCEDURE DIVISION.

       *--- Main entry point: orchestrates initialization, processing, and finalization
       MAIN-LOGIC.
           PERFORM INITIALIZATION
           PERFORM PROCESS-ACCOUNTS
           PERFORM FINALIZATION
           STOP RUN.

       *--- Initialization: open files, set run date, initialize counters
       INITIALIZATION.
           OPEN INPUT ACCT-MASTER-IN
           OPEN INPUT ACCT-TRAN-IN
           OPEN OUTPUT ACCT-MASTER-OUT
           OPEN OUTPUT AUDIT-REPORT-OUT
           PERFORM SET-RUN-DATE
           MOVE 'N' TO WS-EOF-MASTER
           MOVE 'N' TO WS-EOF-TRAN
           PERFORM READ-NEXT-MASTER
           PERFORM READ-NEXT-TRAN
           MOVE ZEROS TO WS-ACCTS-PROCESSED
           MOVE ZEROS TO WS-ACCTS-UPDATED
           MOVE ZEROS TO WS-ACCTS-NO-TRANS
           MOVE ZEROS TO WS-UNKNOWN-TRAN-CNT.

       *--- Set run date from system, fallback to hardcoded if unavailable
       SET-RUN-DATE.
           ACCEPT WS-RUN-DATE FROM DATE YYYYMMDD
           IF WS-RUN-DATE = ZEROS
               MOVE 20241201 TO WS-RUN-DATE
           END-IF.

       *--- Main account processing loop
       PROCESS-ACCOUNTS.
           PERFORM UNTIL WS-EOF-MASTER = 'Y'
               MOVE 'N' TO WS-TRANSACTION-FOUND
               PERFORM MATCH-AND-APPLY-TRANSACTIONS
               IF WS-TRANSACTION-FOUND = 'Y'
                   ADD 1 TO WS-ACCTS-UPDATED
               ELSE
                   ADD 1 TO WS-ACCTS-NO-TRANS
                   PERFORM WRITE-NO-TRANSACTION-DETAIL
               END-IF
               PERFORM WRITE-MASTER-OUT
               ADD 1 TO WS-ACCTS-PROCESSED
               PERFORM READ-NEXT-MASTER
           END-PERFORM.

       *--- For each account, match and apply all transactions
       MATCH-AND-APPLY-TRANSACTIONS.
           MOVE 'N' TO WS-TRANSACTION-APPLIED
           PERFORM UNTIL WS-EOF-TRAN = 'Y'
               IF T-ACCT-NUMBER < M-ACCT-NUMBER
                   PERFORM READ-NEXT-TRAN
               ELSE
                   IF T-ACCT-NUMBER = M-ACCT-NUMBER
                       PERFORM APPLY-TRANSACTION
                       MOVE 'Y' TO WS-TRANSACTION-FOUND
                       PERFORM READ-NEXT-TRAN
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

       *--- Apply transaction logic based on code
       APPLY-TRANSACTION.
           EVALUATE T-TRAN-CODE
               WHEN '01'
                   ADD T-TRAN-AMOUNT TO M-ACCT-BALANCE
                   MOVE 'DEPOSIT' TO DL-STATUS
               WHEN '02'
                   SUBTRACT T-TRAN-AMOUNT FROM M-ACCT-BALANCE
                   MOVE 'WITHDRAWAL' TO DL-STATUS
               WHEN '03'
                   SUBTRACT T-TRAN-AMOUNT FROM M-ACCT-BALANCE
                   MOVE 'FEE' TO DL-STATUS
               WHEN OTHER
                   ADD 1 TO WS-UNKNOWN-TRAN-CNT
                   MOVE 'UNKNOWN TRAN CODE' TO DL-STATUS
           END-EVALUATE
           MOVE T-ACCT-NUMBER TO DL-ACCT-NUMBER
           MOVE M-ACCT-NAME TO DL-ACCT-NAME
           MOVE T-TRAN-CODE TO DL-TRAN-CODE
           MOVE T-TRAN-AMOUNT TO DL-TRAN-AMOUNT
           MOVE M-ACCT-BALANCE TO DL-NEW-BALANCE
           IF T-TRAN-CODE = '01' OR T-TRAN-CODE = '02' OR T-TRAN-CODE = '03'
               MOVE WS-RUN-DATE TO M-LAST-ACTIVITY-DATE
               MOVE 'Y' TO WS-TRANSACTION-APPLIED
           END-IF
           PERFORM WRITE-DETAIL-REPORT.

       *--- Write transaction detail line to audit report
       WRITE-DETAIL-REPORT.
           STRING
               DL-ACCT-NUMBER DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               DL-ACCT-NAME DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               DL-TRAN-CODE DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               DL-TRAN-AMOUNT DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               DL-NEW-BALANCE DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               DL-STATUS DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC.

       *--- Write 'NO TRANSACTIONS' detail line for accounts without transactions
       WRITE-NO-TRANSACTION-DETAIL.
           STRING
               M-ACCT-NUMBER DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               M-ACCT-NAME DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               'NO TRANSACTIONS' DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC.

       *--- Write updated master record
       WRITE-MASTER-OUT.
           MOVE M-ACCT-NUMBER TO OUT-ACCT-NUMBER
           MOVE M-ACCT-NAME TO OUT-ACCT-NAME
           MOVE M-ACCT-BALANCE TO OUT-ACCT-BALANCE
           MOVE M-LAST-ACTIVITY-DATE TO OUT-LAST-ACTIVITY
           MOVE WS-MASTER-OUT-REC TO ACCT-MASTER-OUT-REC
           WRITE ACCT-MASTER-OUT-REC.

       *--- Read next master record, set EOF flag if done
       READ-NEXT-MASTER.
           READ ACCT-MASTER-IN INTO ACCT-MASTER-REC
               AT END
                   MOVE 'Y' TO WS-EOF-MASTER
           END-READ.

       *--- Read next transaction record, set EOF flag if done
       READ-NEXT-TRAN.
           READ ACCT-TRAN-IN INTO ACCT-TRAN-REC
               AT END
                   MOVE 'Y' TO WS-EOF-TRAN
           END-READ.

       *--- Finalization: write summary, close files
       FINALIZATION.
           PERFORM WRITE-SUMMARY-REPORT
           CLOSE ACCT-MASTER-IN
           CLOSE ACCT-TRAN-IN
           CLOSE ACCT-MASTER-OUT
           CLOSE AUDIT-REPORT-OUT.

       *--- Write summary totals to audit report
       WRITE-SUMMARY-REPORT.
           STRING
               'TOTAL ACCOUNTS: ' DELIMITED BY SIZE
               WS-ACCTS-PROCESSED DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC

           STRING
               'UPDATED ACCOUNTS: ' DELIMITED BY SIZE
               WS-ACCTS-UPDATED DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC

           STRING
               'NO TRANSACTION ACCOUNTS: ' DELIMITED BY SIZE
               WS-ACCTS-NO-TRANS DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC

           STRING
               'UNKNOWN TRAN CODES: ' DELIMITED BY SIZE
               WS-UNKNOWN-TRAN-CNT DELIMITED BY SIZE
               INTO WS-RPT-TEXT
           END-STRING
           MOVE WS-RPT-TEXT TO AUDIT-REPORT-REC
           WRITE AUDIT-REPORT-REC.

       END PROGRAM ACCTUPDT-REFACTORED.

Section Explanations:

- File Handling: Modern SELECT/FD syntax is used for all input/output files, with clear organization and naming.
- Data Structures: Records are defined with meaningful names and explicit sizes for clarity and safety. Monetary values use COMP-3 for legacy compatibility but can be migrated to decimal types in future modernization.
- Initialization: All files are opened, counters reset, and run date is set from system or fallback value. This ensures safe startup and auditability.
- Main Logic: Structured PERFORMs replace legacy GO TOs, with modular paragraphs for initialization, processing, and finalization.
- Transaction Application: EVALUATE replaces IF/GO TO for transaction code logic, with clear mapping for deposit, withdrawal, fee, and unknown codes. Last activity date is updated only for valid transactions.
- Reporting: Audit report lines and summary are written using STRING for flexible formatting. 'NO TRANSACTIONS' is reported for accounts without transactions.
- Error Handling: End-of-file flags are managed explicitly, and unknown transaction codes are counted and reported.
- Modularity: Each paragraph has a single responsibility, enabling easier maintenance and future refactoring.

Change Log:

- Replaced all GO TO statements with structured PERFORMs and modular paragraphs.
- Split monolithic paragraphs into focused routines for initialization, transaction application, reporting, and finalization.
- Improved variable naming for clarity and maintainability.
- Added inline comments and section headers for documentation and onboarding.
- Separated business logic from reporting logic for maintainability.
- Externalized run date acquisition, with fallback to hardcoded value for auditability.
- Explicitly managed end-of-file conditions for robust file processing.
- Added summary reporting for processed, updated, no-transaction, and unknown transaction accounts.
- Validated code against key business rules and test cases for correctness and reliability.
