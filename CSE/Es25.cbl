      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 TABELLA.
           05 ARRAY OCCURS 1 TO 100 TIMES DEPENDING ON SIZE-ARRAY
           INDEXED BY I.
               10 NOME PIC A(20).

       01 SIZE-ARRAY USAGE IS INDEX.

       01 GIORNATA.
           05 GG PIC 9(2).
           05 MM PIC 9(2).
           05 AA PIC 9(4).
           05 FILLER-L PIC X(1) VALUES "-".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "INSERISCI LA DATA".

           DISPLAY "GIORNO".
           ACCEPT GG.
           DISPLAY "MESE".
           ACCEPT MM.
           DISPLAY "ANNO".
           ACCEPT AA.

           DISPLAY GG FILLER-L MM FILLER-L AA.

            STOP RUN.


      *---------------------------------- FUNCTION AA ------------------

       CONVERTI-DATA.


      *--------------------------------- FUNCTION AB -------------------
