       IDENTIFICATION DIVISION.
       AUTHOR. KEVIN DE NOTARIIS.
       PROGRAM-ID. ESEMPIO.
       DATE-WRITTEN. 07/05/2021.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               BEGIN DECLARE SECTION
           END-EXEC.
           01 HOSTVARS.
               05 BUFFER           PIC X(1024).
               05 hVarD            PIC S9(5)V99.
               05 hVarC            PIC X(50).
               05 hVarN            PIC 9(12). 
           EXEC SQL
               END DECLARE SECTION
           END-EXEC.
       
           PROCEDURE DIVISION.
     
           STRING 'DRIVER={Devart ODBC Driver for MySQL} ;'
                  'SERVER=localhost;'
                  'PORT=3306;'
                  'DATABASE=prova4;'
                  'USER=root;'
                  'PASSWORD=Domenico9;'
           INTO BUFFER  
           END-STRING
           EXEC SQL
               CONNECT TO :BUFFER
           END-EXEC.
           PERFORM SQLSTATE-CHECK

           MOVE SPACES TO BUFFER.
           STRING 
               'CREATE TABLE TESTPERSON('
               'ID DECIMAL(12,0), '
               'NAME CHAR(50) NOT NULL, '
               'PRIMARY KEY (ID))'
             INTO BUFFER
           END-STRING
           EXEC SQL 
             EXECUTE IMMEDIATE  :BUFFER
           END-EXEC
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
             EXEC SQL 
              INSERT INTO TESTPERSON SET
               ID=:hVarN,
               NAME=:hVarC
             END-EXEC
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
       