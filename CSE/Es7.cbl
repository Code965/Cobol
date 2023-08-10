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
           05 ARRAY PIC 9(2) OCCURS 5 TIMES INDEXED BY I,J.

       01 CONT PIC 9(2) VALUES 0.
       01 TEMP PIC 9(2).
       01 FLAG PIC 9(1).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM RIEMPI-ARRAY.
           PERFORM STAMPA-1.
           PERFORM CALCOLA-OCCORRENZE.

           STOP RUN.

       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           DISPLAY "INSERISCI IL DATO ARRAY 2:  "
           ACCEPT ARRAY(I)

           END-PERFORM.

       STAMPA-1.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
            DISPLAY "VALORE ARRAY1: " ARRAY(I)
           END-PERFORM.

       CALCOLA-OCCORRENZE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5

               MOVE ARRAY(I) TO TEMP

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5

                   IF TEMP = ARRAY(J) THEN
                       COMPUTE CONT = CONT + 1
                       COMPUTE FLAG = 0
                   ELSE
                       COMPUTE FLAG = 1

                   END-IF

               END-PERFORM

               DISPLAY "NUMERO: " ARRAY(I) " OCCORRENZE: " CONT
               INITIALIZE CONT
           END-PERFORM.
