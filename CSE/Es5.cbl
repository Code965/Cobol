      ******************************************************************
      * Riempi un vettore e fai lo shift dei valori a sinistra
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 TABELLA.
         05 ARRAY PIC 9(2) OCCURS 5 TIMES INDEXED BY I.


       01 DATO PIC 9(2).
       01 TEMP PIC 9(2) VALUE 0.
       01 J PIC 9(2) VALUE 0.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE I TO J.
           PERFORM RIEMPI-ARRAY.
           PERFORM STAMPA-ARRAY.


           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5

               PERFORM SHIFT-SINISTRA
               PERFORM STAMPA-ARRAY

           END-PERFORM.









           STOP RUN.

      *Paragrafi

       RIEMPI-ARRAY.
      *Inserimento valori
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT DATO
               MOVE DATO TO ARRAY(I)
           END-PERFORM.

       STAMPA-ARRAY.
      *stampa i valori
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               DISPLAY "VALORE: " ARRAY(I)
           END-PERFORM.

       SHIFT-SINISTRA.
           DISPLAY "SHIFT-SINISTRA"
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 5
           MOVE ARRAY(I) TO ARRAY(I - 1)
           END-PERFORM.
           MOVE 00 TO ARRAY(5).
