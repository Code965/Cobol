
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 TABELLA1.
           05 ARRAY PIC X(100) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.

       01 TABELLA2.
           05 ARRAY2 PIC X(100) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY J.

       01 SIZE-ARRAY USAGE IS INDEX.

       01 TEMP PIC X(100).
       01 PAROLE-A PIC X(100).
       01 PAROLE-B PIC X(100).



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY-A.
              PERFORM RIEMPI-ARRAY-B.
              PERFORM STAMPA-ARRAY-A.
              PERFORM STAMPA-ARRAY-B.
              PERFORM CONFRONTO.

           STOP RUN.

      * ------------------------------------ FUNCION AA ----------------
       RIEMPI-ARRAY-A.
           DISPLAY "ARRAY A"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI LE PAROLE: "
               ACCEPT PAROLE-A
               MOVE PAROLE-A TO ARRAY(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AB ----------------
       RIEMPI-ARRAY-B.
           DISPLAY "ARRAY A"
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY
               DISPLAY "INSERISCI LE PAROLE: "
               ACCEPT PAROLE-B
               MOVE PAROLE-B TO ARRAY2(J)
           END-PERFORM.
      *------------------------------------ FUNCTION AB ----------------
       STAMPA-ARRAY-A.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "VALORE: " ARRAY(I)
           END-PERFORM.
      *------------------------------------ FUNCTION AC ----------------
       STAMPA-ARRAY-B.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY
               DISPLAY "VALORE: " ARRAY2(J)
           END-PERFORM.

      *------------------------------------ FUNCTION -------------------
       CONFRONTO.

           SET I TO 1

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               MOVE ARRAY(I) TO TEMP

               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY

                   IF TEMP = ARRAY2(J) THEN

                       DISPLAY "UGUALI: " ARRAY2(J)

                   END-IF

               END-PERFORM

           END-PERFORM.
