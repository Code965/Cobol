      ******************************************************************
      * caricare un vettore di N posizioni, calcolare il quadrato di N
      * numeri e poi inserirli in QUADR, ordinandolo.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 TABELLA.
           05 NUM PIC 9(3) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.

       01 TABELLA2.
           05 QUADR PIC 9(3) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY J.

       01 RISULTATO PIC 9(3).

       01 SIZE-ARRAY USAGE IS INDEX.

       01 TEMP PIC 9(3).
       01 VAL PIC 9(3).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY.
              PERFORM STAMPA-ARRAY.
              PERFORM QUADRATI.
              PERFORM SORT-ARRAY.
              PERFORM STAMPA-ARRAY-2.

           STOP RUN.

      * ------------------------------------ FUNCION AA ----------------
       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT VAL
               MOVE VAL TO NUM(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AB ----------------
       STAMPA-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "VALORE NUM: " NUM(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AC ----------------
       QUADRATI.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY - 1

               COMPUTE RISULTATO = NUM(I) * NUM(I)

               MOVE RISULTATO TO QUADR(I)

               DISPLAY RISULTATO

           END-PERFORM.

      *---------------------------------------- FUNCTION AD ------------

       STAMPA-ARRAY-2.
           DISPLAY "QUADRATI".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "VALORE QUADR" QUADR(I)
           END-PERFORM.

      *--------------------------------------- FUNCTION AE -------------
       SORT-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY - 1

               IF QUADR(I) > QUADR(I + 1) THEN

                   MOVE QUADR(I) TO TEMP
                   MOVE QUADR(I + 1) TO QUADR(I)
                   MOVE TEMP TO QUADR(I + 1)

               END-IF


           END-PERFORM.
