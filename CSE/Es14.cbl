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
           05 ARRAY PIC X(99) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.

       01 SIZE-ARRAY USAGE IS INDEX.

       01 VAL PIC X(99).
       01 FILLER-1 PIC 9(1) VALUE 0.
       01 TEMP PIC X(99).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


           PERFORM DIMENSIONE.
           PERFORM RIEMPI-ARRAY.
           PERFORM ELENCO-PAROLE.
           PERFORM TROVA-LETTERA.
      *     PERFORM SEARCH-WORD.


           STOP RUN.

      *--------------------------------------- FUNCTION AA -------------
      *LE STAMPA
       ELENCO-PAROLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "ELENCO DI PAROLE: " ARRAY(I)
           END-PERFORM.
      *--------------------------------------- FUNCTION AC -------------
      *INSERISCE LE PAROLE NELL'ARRAY
       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI LA STRINGA: "
               ACCEPT VAL
               MOVE VAL TO ARRAY(I)
           END-PERFORM.
      *--------------------------------------- FUNCTION AD -------------
      * INSERISCE LA DIMENSIONE DELL'ARRAY VARIABILE
       DIMENSIONE.
           DISPLAY "INSERISCI LA DIMENSIONE DELL'ARRAY".
           ACCEPT SIZE-ARRAY.
      *---------------------------------------- FUNCTION AE ------------
       TROVA-LETTERA.
      * VERSIONE ALTENATIVA DEL CERCARE LA PAROLA

           DISPLAY "INSERISCI IL VALORE DA CERCARE".
           ACCEPT VAL.

           SET I TO 1.
           SEARCH ARRAY AT END DISPLAY " NON TROVATO"
           WHEN ARRAY(I) = VAL
           DISPLAY "TROVATA: " ARRAY(I)
           END-SEARCH.
