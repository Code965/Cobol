      ******************************************************************
      * Si scriva un programma in COBOL  per generare un terzo vettore
      * che contiene l'intersezione tra due vettori.
      * Tale verrote deve contenere i numeri presenti in entrambi i vettori dati
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
      *TABELLA IN CUI RACCOLGO TUTTO
       01 TABELLA_ARRAY1.
               05 ARRAY1 PIC 9(2) OCCURS 5 TIMES INDEXED BY I.

      *TABELLA IN CUI METTO I VALORI PULITI
       01 TABELLA_ARRAY2.
               05 ARRAY2 PIC 9(2) OCCURS 5 TIMES INDEXED BY J.

       01 TABELLA_ARRAY3.
               05 ARRAY3 PIC 9(2) OCCURS 5 TIMES INDEXED BY Z.

       01 TEMP3 PIC 9(2) VALUE 0.
       01 TEMP4 PIC 9(2) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM RIEMPI-ARRAY1.
           PERFORM RIEMPI-ARRAY2.

           PERFORM STAMPA-1.
           PERFORM STAMPA-2.

           PERFORM CONFRONTO.

           PERFORM SORT-ARRAY3.

           PERFORM STAMPA-3.

       STOP RUN.

       RIEMPI-ARRAY1.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           DISPLAY "INSERISCI IL DATO ARRAY1: "
           ACCEPT ARRAY1(I)

           END-PERFORM.

       RIEMPI-ARRAY2.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
           DISPLAY "INSERISCI IL DATO ARRAY 2:  "
           ACCEPT ARRAY2(J)

           END-PERFORM.

       STAMPA-1.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
            DISPLAY "VALORE ARRAY1: " ARRAY1(I)

           END-PERFORM.

       STAMPA-2.

            PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
            DISPLAY "VALORE ARRAY2: " ARRAY2(J)

           END-PERFORM.

       STAMPA-3.

            DISPLAY "STAMPA"
            PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 5
            DISPLAY "ARRAY3: " ARRAY3(Z)

           END-PERFORM.

       CONFRONTO.

           INITIALIZE TEMP3.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5 - 1

               MOVE ARRAY1(I) TO TEMP3

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5 - 1

                  IF TEMP3 = ARRAY2(J) THEN
                      MOVE TEMP3 TO ARRAY3(J)
                  END-IF

               END-PERFORM

           END-PERFORM.


       SORT-ARRAY3.

           INITIALIZE TEMP4.

           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 5 - 1


               IF ARRAY3(Z) > ARRAY3( Z + 1 ) THEN

                   MOVE ARRAY3(Z) TO TEMP4
                   MOVE ARRAY3( Z + 1 ) TO ARRAY3(Z)
                   MOVE TEMP4 TO ARRAY3( Z + 1 )

               END-IF
           END-PERFORM.
