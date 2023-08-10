       IDENTIFICATION DIVISION.
       AUTHOR. KEVIN DE NOTARIIS.
       PROGRAM-ID. ESEMPIO.
       DATE-WRITTEN. 07/05/2021.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      **********************************************************************
      *******                EMBEDDED SQL VARIABLES                  *******
       01 SQLCA.
           05 SQLSTATE PIC X(5).
              88  SQL-SUCCESS           VALUE '00000'.
              88  SQL-RIGHT-TRUNC       VALUE '01004'.
              88  SQL-NODATA            VALUE '02000'.
              88  SQL-DUPLICATE         VALUE '23000' THRU '23999'.
              88  SQL-MULTIPLE-ROWS     VALUE '21000'.
              88  SQL-NULL-NO-IND       VALUE '22002'.
              88  SQL-INVALID-CURSOR-STATE VALUE '24000'.
           05 FILLER   PIC X.
           05 SQLVERSN PIC 99 VALUE 02.
           05 SQLCODE  PIC S9(9) COMP-5.
           05 SQLERRM.
               49 SQLERRML PIC S9(4) COMP-5.
               49 SQLERRMC PIC X(486).
           05 SQLERRD OCCURS 6 TIMES PIC S9(9) COMP-5.
       01 SQLV.
           05 SQL-ARRSZ  PIC S9(9) COMP-5 VALUE 2.
           05 SQL-COUNT  PIC S9(9) COMP-5.
           05 SQL-ADDR   POINTER OCCURS 2 TIMES.
           05 SQL-LEN    PIC S9(9) COMP-5 OCCURS 2 TIMES.
           05 SQL-TYPE   PIC X OCCURS 2 TIMES.
           05 SQL-PREC   PIC X OCCURS 2 TIMES.
      **********************************************************************
       01 SQL-STMT-0.
           05 SQL-IPTR   POINTER.
           05 SQL-PREP   PIC X VALUE 'N'.
           05 SQL-OPT    PIC X VALUE SPACE.
           05 SQL-PARMS  PIC S9(4) COMP-5 VALUE 2.
           05 SQL-STMLEN PIC S9(4) COMP-5 VALUE 38.
           05 SQL-STMT   PIC X(38) VALUE 'INSERT INTO TESTPERSON SET ID=
      -    '?,NAME=?'.
      **********************************************************************
      *******          PRECOMPILER-GENERATED VARIABLES               *******
       01 SQLV-GEN-VARS.
           05 SQL-VAR-0002  PIC S9(13) COMP-3.
      *******       END OF PRECOMPILER-GENERATED VARIABLES           *******
      **********************************************************************
      *    EXEC SQL
      *        BEGIN DECLARE SECTION
      *    END-EXEC.
           01 HOSTVARS.
               05 BUFFER           PIC X(1024).
               05 hVarD            PIC S9(5)V99.
               05 hVarC            PIC X(50).
               05 hVarN            PIC 9(12).
      *    EXEC SQL
      *        END DECLARE SECTION
      *    END-EXEC.

           PROCEDURE DIVISION.

           STRING 'DRIVER={Devart ODBC Driver for MySQL} ;'
                  'SERVER=localhost;'
                  'PORT=3306;'
                  'DATABASE=prova4;'
                  'USER=root;'
                  'PASSWORD=Domenico9;'
           INTO BUFFER
           END-STRING
      *    EXEC SQL
      *        CONNECT TO :BUFFER
      *    END-EXEC.
           MOVE 1024 TO SQL-LEN(1)
           CALL 'OCSQL'    USING BUFFER
                               SQL-LEN(1)
                               SQLCA
           END-CALL
                   .
           PERFORM SQLSTATE-CHECK

           MOVE SPACES TO BUFFER.
           STRING
               'CREATE TABLE TESTPERSON('
               'ID DECIMAL(12,0), '
               'NAME CHAR(50) NOT NULL, '
               'PRIMARY KEY (ID))'
             INTO BUFFER
           END-STRING
      *    EXEC SQL
      *      EXECUTE IMMEDIATE  :BUFFER
      *    END-EXEC
           MOVE 1024 TO SQL-LEN(1)
           CALL 'OCSQLIMM' USING BUFFER
                               SQL-LEN(1)
                               SQLCA
           END-CALL
           IF SQLSTATE='42S01'
             DISPLAY ' Table TESTPERSON already exists.'
           ELSE
             PERFORM SQLSTATE-CHECK
             DISPLAY ' created Table TESTPERSON'
             PERFORM INSDATAPERSON
           END-IF

           STOP RUN.

       INSDATAPERSON.

           MOVE 0 TO hVarN.
           PERFORM UNTIL hVarN > 2
             COMPUTE hVarN = hVarN + 1
             STRING 'Testpers '
                    hVarN
               INTO hVarC
             END-STRING
      *      EXEC SQL
      *       INSERT INTO TESTPERSON SET
      *        ID=:hVarN,
      *        NAME=:hVarC
      *      END-EXEC
           IF SQL-PREP OF SQL-STMT-0 = 'N'
               SET SQL-ADDR(1) TO ADDRESS OF
                 SQL-VAR-0002
               MOVE '3' TO SQL-TYPE(1)
               MOVE 7 TO SQL-LEN(1)
               MOVE X'00' TO SQL-PREC(1)
               SET SQL-ADDR(2) TO ADDRESS OF
                 HVARC
               MOVE 'X' TO SQL-TYPE(2)
               MOVE 50 TO SQL-LEN(2)
               MOVE 2 TO SQL-COUNT
               CALL 'OCSQLPRE' USING SQLV
                                   SQL-STMT-0
                                   SQLCA
           END-IF
           MOVE HVARN
             TO SQL-VAR-0002
           CALL 'OCSQLEXE' USING SQL-STMT-0
                               SQLCA
             PERFORM SQLSTATE-CHECK
             DISPLAY 'INSERTED '
             DISPLAY '  Person ' hVarN ' NAME ' hVarC
           END-PERFORM

           EXIT.

       SQLSTATE-CHECK.
           IF SQLCODE < 0
                      DISPLAY 'SQLSTATE='  SQLSTATE,
                              ', SQLCODE=' SQLCODE
              IF SQLERRML > 0
                 DISPLAY 'SQL Error message:' SQLERRMC(1:SQLERRML)
              END-IF
              MOVE SQLCODE TO RETURN-CODE
              STOP RUN
           ELSE IF SQLCODE > 0 AND NOT = 100
                      DISPLAY 'SQLSTATE='  SQLSTATE,
                              ', SQLCODE=' SQLCODE
              IF SQLERRML > 0
                 DISPLAY 'SQL Warning message:' SQLERRMC(1:SQLERRML)
              END-IF
           END-IF.

           EXIT.

      **********************************************************************
      *  : ESQL for GnuCOBOL/OpenCobol Version 2 (2021.05.29) Build May 29 2021

      *******               EMBEDDED SQL VARIABLES USAGE             *******
      *  BUFFER                   IN USE CHAR(1024)
      *  HOSTVARS             NOT IN USE
      *  HOSTVARS.BUFFER      NOT IN USE
      *  HOSTVARS.HVARC       NOT IN USE
      *  HOSTVARS.HVARD       NOT IN USE
      *  HOSTVARS.HVARN       NOT IN USE
      *  HVARC                    IN USE CHAR(50)
      *  HVARD                NOT IN USE
      *  HVARN                    IN USE THROUGH TEMP VAR SQL-VAR-0002 DECIMAL(13,0)
      **********************************************************************
