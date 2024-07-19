      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-2.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCKS-FILE-IN
           ASSIGN TO '..\STOCKS.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      * Input file for stocks data

           SELECT PORTFOLIO-FILE-IN
           ASSIGN TO '..\PORTFOLIO.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      * Input file for portfolio data


           SELECT REPORT-FILE-IN
           ASSIGN TO '..\REPORT.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
      * Output file for the generated report

       DATA DIVISION.
       FILE SECTION.
       FD STOCKS-FILE-IN.
       01 STOCKS-RECORD-IN.
           05 STOCKS-SYMBOL-IN PIC X(7).
           05 STOCKS-NAME-IN PIC X(25).
           05 STOCKS-PRICE-IN PIC 9(4)V99.
      * Structure of a stock record in the input file


       FD PORTFOLIO-FILE-IN.
       01 PORTFOLIO-RECORD-IN.
           05 PRSTOCK-SYMBOL-IN PIC X(7).
           05 PRSTOCK-SHARES-IN PIC 9(5).
           05 PRAVERAGE-COST-IN PIC 9(4)V99.
      * Structure of a portfolio record in the input file


       FD REPORT-FILE-IN.
       01 REPORT-RECORD-IN PIC X(100).
      * Structure of a report record in the output file

       WORKING-STORAGE SECTION.
       01 STOCKS-RECORD-OUT.
           05 STOCKS-TABLE OCCURS 20 TIMES.
               10 STOCK-SYMBOL-OUT PIC X(7).
               10 STOCK-NAME-OUT PIC X(25).
               10 STOCK-PRICE-OUT PIC 9(4)V99.
      * Table to hold stock data

       01 PORTFOLIO-RECORD-OUT.
           05 PORTFOLIO-TABLE OCCURS 7 TIMES.
              10 PRSTOCK-SYMBOL-OUT PIC X(7).
              10 PRSTOCK-SHARES-OUT PIC 9(5).
              10 PRAVERAGE-COST-OUT PIC 9(4)V99.
      * Table to hold portfolio data


       01 REPORT-FILE-MARGIN.
        05 FILLER PIC X(35) VALUE '==================================='.
        05 FILLER PIC X(35) VALUE '==================================='.
        05 FILLER PIC X(35) VALUE '==================================='.
      * Margins for the report file


       01 REPORT-FILE-COUNTER.
         05 REPORT-READ-TEXT PIC X(14) VALUE 'Records Read: '.
         05 REPORT-READ-COUNTER PIC Z9.
         05 FILLER PIC X(5).
         05 REPORT-WRITE-TEXT PIC X(17) VALUE 'Reports Written: '.
         05 REPORT-WRITE-COUNTER PIC Z9.
      * Counters for records read and reports written

       01 REPORT-FILE-HEADER.
          05 FILLER PIC X(25) VALUE 'STOCK NAME'.
          05 FILLER PIC X(9) VALUE '#SHARES'.
          05 FILLER PIC X(12) VALUE 'UNIT COST'.
          05 FILLER PIC X(13) VALUE 'AT CLOSING'.
          05 FILLER PIC X(13) VALUE 'COST BASE'.
          05 FILLER PIC X(15) VALUE 'MARKET VALUE'.
          05 FILLER PIC X(10) VALUE 'GAIN/LOSS'.
      * Header for the report file


       01 REPORT-FILE-RECORD-OUT.
          05 REPORT-STOCK-NAME PIC X(25).
          05 REPORT-STOCK-SHARE PIC Z(4)9.
          05 FILLER PIC X(3).
          05 REPORT-UNIT-COST PIC $$,$$9.99.
          05 FILLER PIC X(4).
          05 REPORT-AT-CLOSING PIC $$,$$9.99.
          05 FILLER PIC X(3).
          05 REPORT-COST-BASE PIC $$$$,$$9.99.
          05 FILLER PIC X(3).
          05 REPORT-MARKET-VALUE PIC $$$$,$$9.99.
          05 FILLER PIC X(3).
          05 REPORT-GAIN-LOSS PIC $$$$,$$9.99-.
      * Structure of a report record to be written

       01 REPORT-COST-BASE-NUM PIC 9(6)V99.
       01 REPORT-COST-MARKET-NUM PIC 9(6)V99.
       01 REPORT-GAIN-LOSS-DIF PIC S9(6)V99.

       01 STOCK-INPUT-FLAG PIC X.
           88 NO-MORE-STOCK VALUE 'N'.
      * Flags and variables for report calculation

       01 REPORT-WRITE-INC PIC 99 VALUE 0.
       01 REPORT-READ-INC PIC 99 VALUE 0.
       01 STOCK-INDEX PIC 99.
       01 PORTFOLIO-INDEX PIC 9.
      * Indexes and counters for processing

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM INITIALIZE-FILES.
            PERFORM PROCESS-FILES.
            PERFORM CREATE-REPORT-FILE.
            PERFORM CLOSE-FILES.
            STOP RUN.
      * Main procedure to initialize, process files, create report, and close files

       INITIALIZE-FILES.
       OPEN INPUT STOCKS-FILE-IN.
       OPEN INPUT PORTFOLIO-FILE-IN.
       OPEN OUTPUT REPORT-FILE-IN.
      * Subroutine to open input and output files

       PROCESS-FILES.
       PERFORM LOAD-STOCKS.
       PERFORM LOAD-PORTFOLIO.
      * Subroutine to load stocks and portfolio data

       LOAD-STOCKS.

           READ STOCKS-FILE-IN AT END MOVE 'N' TO STOCK-INPUT-FLAG.
      * Read stocks file and set end flag if no more records

           PERFORM LOAD-STOCKS-TABLE VARYING STOCK-INDEX FROM 1 BY 1
           UNTIL(STOCK-INDEX > 20 OR NO-MORE-STOCK).
      * Load stocks into table until end of file or table is full


       LOAD-STOCKS-TABLE.
           MOVE STOCKS-SYMBOL-IN TO STOCK-SYMBOL-OUT(STOCK-INDEX).
           MOVE STOCKS-NAME-IN TO STOCK-NAME-OUT(STOCK-INDEX).
           MOVE STOCKS-PRICE-IN TO STOCK-PRICE-OUT(STOCK-INDEX).
           READ STOCKS-FILE-IN AT END MOVE 'N' TO STOCK-INPUT-FLAG.
      * Move stock data to table and read next record

       LOAD-PORTFOLIO.

           READ PORTFOLIO-FILE-IN AT END MOVE 'N' TO STOCK-INPUT-FLAG.
      * Read portfolio file and set end flag if no more records


           PERFORM LOAD-PORTFOLIO-TABLE VARYING PORTFOLIO-INDEX FROM 1
           BY 1 UNTIL(PORTFOLIO-INDEX > 7).
      * Load portfolio into table until end of file or table is full

       LOAD-PORTFOLIO-TABLE.
           MOVE PRSTOCK-SYMBOL-IN TO PRSTOCK-SYMBOL-OUT(PORTFOLIO-INDEX).
           MOVE PRSTOCK-SHARES-IN TO PRSTOCK-SHARES-OUT(PORTFOLIO-INDEX).
           MOVE PRAVERAGE-COST-IN TO PRAVERAGE-COST-OUT(PORTFOLIO-INDEX).
           READ PORTFOLIO-FILE-IN AT END MOVE 'N' TO STOCK-INPUT-FLAG.
      * Move portfolio data to table and read next record


       CREATE-REPORT-FILE.
           WRITE REPORT-RECORD-IN FROM REPORT-FILE-MARGIN
           WRITE REPORT-RECORD-IN FROM REPORT-FILE-HEADER
           WRITE REPORT-RECORD-IN FROM REPORT-FILE-MARGIN
           PERFORM VARYING PORTFOLIO-INDEX FROM 1 BY 1
           UNTIL PORTFOLIO-INDEX > 7
           PERFORM CHECKING-PROCESS
           ADD 1 TO REPORT-READ-INC
           END-PERFORM.
      * Write report margins and header, then process each portfolio entry


           WRITE REPORT-RECORD-IN FROM REPORT-FILE-MARGIN.
           MOVE REPORT-READ-INC TO REPORT-READ-COUNTER
           MOVE REPORT-WRITE-INC TO  REPORT-WRITE-COUNTER.
           WRITE REPORT-RECORD-IN FROM REPORT-FILE-COUNTER.
      * Write final report margin and counters

       CHECKING-PROCESS.
       DISPLAY "CHECKING SULOD"
       PERFORM VARYING STOCK-INDEX FROM 1 BY 1
       UNTIL STOCK-INDEX > 20
      * Check each stock against portfolio and calculate report values


        IF PRSTOCK-SYMBOL-OUT(PORTFOLIO-INDEX) =
            STOCK-SYMBOL-OUT(STOCK-INDEX)
            DISPLAY 'Match found for stock symbol: '
            STOCK-SYMBOL-OUT(STOCK-INDEX)" "
            STOCK-NAME-OUT(STOCK-INDEX)
            STOCK-PRICE-OUT(STOCK-INDEX)
      * If stock symbol matches, display match information


            MOVE STOCK-NAME-OUT(STOCK-INDEX) TO REPORT-STOCK-NAME

            MOVE PRSTOCK-SHARES-OUT(PORTFOLIO-INDEX) TO
            REPORT-STOCK-SHARE

            MOVE PRAVERAGE-COST-OUT(PORTFOLIO-INDEX) TO REPORT-UNIT-COST

            MOVE STOCK-PRICE-OUT(STOCK-INDEX) TO REPORT-AT-CLOSING

            MULTIPLY PRSTOCK-SHARES-OUT(PORTFOLIO-INDEX)
             BY PRAVERAGE-COST-OUT(PORTFOLIO-INDEX)
             GIVING REPORT-COST-BASE-NUM

            MOVE REPORT-COST-BASE-NUM TO REPORT-COST-BASE

            MULTIPLY PRSTOCK-SHARES-OUT(PORTFOLIO-INDEX) BY
             STOCK-PRICE-OUT(STOCK-INDEX) GIVING REPORT-COST-MARKET-NUM

            MOVE REPORT-COST-MARKET-NUM TO REPORT-MARKET-VALUE

            SUBTRACT REPORT-COST-BASE-NUM FROM REPORT-COST-MARKET-NUM
             GIVING REPORT-GAIN-LOSS-DIF


           MOVE REPORT-GAIN-LOSS-DIF TO REPORT-GAIN-LOSS
      * Calculate cost base, market value, and gain/loss for report


           WRITE REPORT-RECORD-IN FROM REPORT-FILE-RECORD-OUT
           ADD 1 TO REPORT-WRITE-INC
      * Write report record and increment write counter

           END-IF

        END-PERFORM.

       CLOSE-FILES.

        CLOSE STOCKS-FILE-IN.
        CLOSE PORTFOLIO-FILE-IN.
        CLOSE REPORT-FILE-IN.
      * Close all files at the end of processing
       END PROGRAM PROJECT-2.
