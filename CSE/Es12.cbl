      ******************************************************************
      * DETERMINARE L'ELEMENTO MAGGIORE DI UN ARRAY
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
       01 MAX PIC 9(3) VALUES ZERO.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY.
              PERFORM STAMPA-ARRAY.
              PERFORM MAGGIORE.

           STOP RUN.

       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT VAL
               MOVE VAL TO ARRAY(I)

           END-PERFORM.

       STAMPA-ARRAY.


           PERFORM VARYING I FROM 0 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "VALORE: " ARRAY(I)

           END-PERFORM.

       MAGGIORE.
           PERFORM VARYING I FROM 0 BY 1 UNTIL I > SIZE-ARRAY

               IF MAX < ARRAY(I) THEN

                    MOVE ARRAY(I) TO MAX

               END-IF

           END-PERFORM.

           DISPLAY "MAGGIORE: " MAX "INDICE" I.
