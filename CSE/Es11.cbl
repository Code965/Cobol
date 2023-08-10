      ******************************************************************
      * DETERMINARE SE UN ELEMENTO DI UN ARRAY è PARI O DISPARI
      ******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 TABELLA.
           05 ARRAY PIC 9(3) OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.

       01 SIZE-ARRAY USAGE IS INDEX.
       01 VAL PIC 9(3).
       01 RISULTATO PIC 9(3).




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY.
              PERFORM STAMPA-ARRAY.
              PERFORM PARI-DISP.

           STOP RUN.

       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT VAL
               MOVE VAL TO ARRAY(I)

           END-PERFORM.

       STAMPA-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "VALORE: " ARRAY(I)

           END-PERFORM.

       PARI-DISP.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               COMPUTE RISULTATO = ARRAY(I) % 2
               DISPLAY "VALORE: "ARRAY(I) "RIS" RISULTATO
               IF RISULTATO NOT = 0 THEN
                   DISPLAY "DISPARI"
               ELSE
                   DISPLAY "PARI"
               END-IF
           END-PERFORM.
