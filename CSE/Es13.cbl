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
       01 MIN PIC 9(3) VALUE ZERO.
       01 INDICE-MAGGIORE PIC 9(3) VALUE ZERO.
       01 INDICE-MINORE PIC 9(3) VALUE ZERO.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
              DISPLAY "INSERISCI LA LUNGHEZZA DEL TUO ARRAY".
              ACCEPT SIZE-ARRAY.

              PERFORM RIEMPI-ARRAY.
              PERFORM STAMPA-ARRAY.
              PERFORM MAGGIORE.
              PERFORM MINORE.


           STOP RUN.
      *------------------------------------- FUNCTION AA ----------------------
      * RIEMPIE L'ARRAY
       RIEMPI-ARRAY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI IL VALORE: "
               ACCEPT VAL
               MOVE VAL TO ARRAY(I)

           END-PERFORM.
      *-------------------------------------- FUNCTION AB ---------------------
      * STAMPA IL CONTENUTO DELLA TABELLA
       STAMPA-ARRAY.


           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "VALORE: " ARRAY(I)

           END-PERFORM.
      *-------------------------------------- FUNCTION AC ---------------------
       MAGGIORE.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF MAX < ARRAY(I) THEN

                    MOVE ARRAY(I) TO MAX
                    MOVE I TO INDICE-MAGGIORE
               END-IF

           END-PERFORM.

           DISPLAY "MAGGIORE: " MAX "INDICE" INDICE-MAGGIORE.
      *-------------------------------------- FUNCTION AD ---------------------
       MINORE.
      * MI SERVE LA PRIMA POSIZIONE DELL'ARRAY COME RIFERIMENTO
      * SE USASSI 0 NON FUNZIONEREBBE

           MOVE ARRAY(1) TO MIN.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
      *SE I VALORI , IN SUCCESSIONE, DELL'ARRAY SONO < DEL PRIMO
                   IF ARRAY(I) < MIN THEN
      *ALLORA LI INSERISCO
                       MOVE ARRAY(I) TO MIN

                   END-IF

           END-PERFORM.

           DISPLAY "MINORE: " MIN "INDICE" INDICE-MINORE.
