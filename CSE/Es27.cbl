
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 DATABASE.
           05 ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON SIZE-ARRAY
              INDEXED BY I.
              10 NOME PIC A(20).
              10 COGNOME PIC A(20).

       01 LISTA_PERSONE.
           05 ARRAY2 OCCURS 1 TO 100 TIMES DEPENDING ON SIZE-ARRAY2
              INDEXED BY I.
               10 NOME PIC A(20).
               10 COGNOME PIC A(20).


       01 SIZE-ARRAY USAGE IS INDEX.
       01 SIZE-ARRAY2 USAGE IS INDEX.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world"
            STOP RUN.






      *FUNZIONI

      *------------------------------------ FUNCTION AA ----------------
       INSERISCI-UTENTE.



      *------------------------------------ FUNCTION AB ----------------
       VERIFICA-LOGIN-PASSWORD.



      *------------------------------------ FUNCTION AC ----------------
       MODIFICA-UTENTE.

      *------------------------------------- FUNCTION AD ---------------
