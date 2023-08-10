      ******************************************************************
      * Costruire un vettore di N posizioni, inserendo in goni elemento
      * il valore del suo indice
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



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
           ACCEPT SIZE-ARRAY.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               MOVE I TO ARRAY(I)

           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "VALORE: " ARRAY(I)

           END-PERFORM.



            STOP RUN.
