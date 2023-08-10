       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *ARRAY MULTIDIMENSIONE
       01 TABELLA.
           05 CITTA OCCURS 2 TIMES INDEXED BY I.
               10 NOME PIC A(2).
               10 GIORNO PIC 9(2).

               10 TEMPERATURA OCCURS 2 TIMES INDEXED BY J.
                   15 TEMP_MAX PIC 9(3).
                   15 TEMP_MIN PIC 9(3).

       01 NOME_CITTA PIC A(2).
       01 GIORNATA PIC 9(2).
       01 TEMP_MAX_ARRAY PIC 9(3).
       01 TEMP_MIN_ARRAY PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.







           STOP RUN.


      *---------------------------- FUNCTION AA ------------------------
