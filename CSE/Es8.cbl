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
      *CREARE UNA TABELLA DI DIMENSIONI VARIABILI
      *GLI STO DICENDO CHE L'ARRAY HA OCCORRENZE DA 1 A 100
      *E CHE LA SUA DIMENSIONE DIPENDE DA SIZE-ARRAY
      *AVENDO UN INDICE

       01 TABELLA.
           05 ARRAY1 PIC 9(3) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY1
           INDEXED BY I.

       01  TABELLA2.
            05 ARRAY2 PIC 9(3) OCCURS 1 TO 100 TIMES
            DEPENDING ON SIZE-ARRAY2
            INDEXED BY J.

       01  TABELLA3.
            05 ARRAY3 PIC 9(3) OCCURS 1 TO 100 TIMES
            DEPENDING ON SIZE-ARRAY3
            INDEXED BY Z.

       01  SIZE-ARRAY1 USAGE IS INDEX.
       01  SIZE-ARRAY2 USAGE IS INDEX.
       01  SIZE-ARRAY3 USAGE IS INDEX.

       01  TEMP PIC 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "INSERISCI LA DIMENSIONE DEL PRIMO ARRAY"
           ACCEPT SIZE-ARRAY1.
           DISPLAY "INSERISCI LA DIMENSIONE DEL SECONDO ARRAY"
           ACCEPT SIZE-ARRAY2.

      *DIMENSIONE DEL TERZO ARRAY
           COMPUTE SIZE-ARRAY3 = SIZE-ARRAY1 + SIZE-ARRAY2.

           DISPLAY "SIZE1" SIZE-ARRAY1
           DISPLAY "SIZE2" SIZE-ARRAY2
           DISPLAY "SIZE3" SIZE-ARRAY3

           PERFORM RIEMPI-ARRAY1.
           PERFORM RIEMPI-ARRAY2.
           PERFORM RIEMPI-ARRAY3.
           PERFORM SORT-ARRAY.
           PERFORM STAMPA-ARRAY.


           STOP RUN.

       RIEMPI-ARRAY1.

           DISPLAY "ARRAY 1"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY1

              DISPLAY "INSERISCI UN VALORE: " I
              ACCEPT ARRAY1(I)

           END-PERFORM.

       RIEMPI-ARRAY2.
           DISPLAY "ARRAY 2"
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY2

              DISPLAY "INSERISCI UN VALORE: " J
              ACCEPT ARRAY2(J)

           END-PERFORM.

       RIEMPI-ARRAY3.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY1
               MOVE ARRAY1(I) TO ARRAY3(I)
           END-PERFORM.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY2
               MOVE ARRAY2(J) TO ARRAY3(J)
           END-PERFORM.


       SORT-ARRAY.

           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > SIZE-ARRAY3 - 1

               IF ARRAY3(Z) > ARRAY3( Z + 1 ) THEN

                   MOVE ARRAY3(Z) TO TEMP
                   MOVE ARRAY3( Z + 1 ) TO ARRAY3(Z)
                   MOVE TEMP TO ARRAY3( Z  + 1 )

               END-IF
           END-PERFORM.

       STAMPA-ARRAY.
      *stampa i valori
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > SIZE-ARRAY3
               DISPLAY "VALORE: " ARRAY3(Z)
           END-PERFORM.
