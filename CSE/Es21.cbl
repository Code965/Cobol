       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 STUDENTI.
           05 NOME_STUDENTE PIC A(20).
           05 COGNOME_STUDENTI PIC A(20).
           05 PUNTEGGI PIC 9(2).

       01 VALORE-RANDOM PIC S9V9(3).
       01 VALORE-RANDOM-ORALE PIC S9V9(3).
       01 TEMP PIC S9V9(3).
       01 CONTATORE PIC 9(2).

       01 MEDIA PIC S9V9(3).

       01 SIZE-ARRAY USAGE IS INDEX.


      *TUTTI GLI STUDENTI SI PRENOTANO
       01  PRENOTAZIONE_ESAME.
           05 ARRAY OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY INDEXED BY I.
               10 NOME PIC A(20).
               10 COGNOME PIC A(20).
               10 VALUTAZIONE PIC 9(2).


      *SOLO GLI STUDENTI CHE HANNO PASSATO LO SCRITTO
       01  IDONEI.
           05 ARRAY OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY INDEXED BY J.
               10 NOME_IDONEO PIC A(20).
               10 COGNOME_IDONEO PIC A(20).
               10 PUNTEGGIO PIC 9(2).

       01  GRADUATORIA.
           05 ARRAY OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY INDEXED BY Z.
               10 NOME_IDONEO_GRAD PIC A(20).
               10 COGNOME_IDONEO_GRAD  PIC A(20).
               10 PUNTEGGIO_SCRITTO PIC 9(2).
               10 PUNTEGGIO_ORALE PIC 9(2).
               10 MEDIA_VAL PIC S9V9(2).


       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           DISPLAY "QUANTI STUDENTI CI SONO NEL CORSO?".
           ACCEPT SIZE-ARRAY.

           PERFORM REGISTRAZIONE-APPELLO.
           PERFORM ELENCO-PRENOTATI.
           PERFORM ESAME.
           PERFORM CORREZIONE.
           PERFORM ESAME-ORALE.
           PERFORM PROMOSSI.

           STOP RUN.


      *---------------------------- FUNCTION AA ------------------------

       REGISTRAZIONE-APPELLO.

           PERFORM VARYING I FROM 1 BY 1 UNTIL  I > SIZE-ARRAY

               DISPLAY "INSERISCI IL TUO NOME: "
               ACCEPT NOME_STUDENTE
               MOVE NOME_STUDENTE TO NOME(I)
               DISPLAY "INSERISCI IL TUO COGNOME: "
               ACCEPT COGNOME_STUDENTI
               MOVE COGNOME_STUDENTI TO COGNOME(I)

               MOVE 0 TO VALUTAZIONE(I)

           END-PERFORM.

      *---------------------------- FUNCTION AB ------------------------

       ELENCO-PRENOTATI.

           DISPLAY "ELENCO PRENOTATI".
           DISPLAY "********************".

           PERFORM VARYING I FROM 1 BY 1 UNTIL  I > SIZE-ARRAY

              DISPLAY "NOME: " NOME(I)
                      "COGNOME: " COGNOME(I)
                      "VALUTAZIONE: " VALUTAZIONE(I)
           END-PERFORM.

      *---------------------------- FUNCTION AC ------------------------
      * VALORIZZA I CAMPI VALUTAZIONE DELL'ARRAY1
       ESAME.

           DISPLAY "ESAME".
           DISPLAY "********************".

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

                COMPUTE VALORE-RANDOM = FUNCTION RANDOM * 30
                MOVE VALORE-RANDOM TO VALUTAZIONE(I)

           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "NOME: " NOME(I)
                       "COGNOME: "  COGNOME(I)
                       "PUNTEGGIO: " VALUTAZIONE(I)
           END-PERFORM.

      *--------------------------- FUNCTION AD -------------------------
       CORREZIONE.

           DISPLAY "CORREZIONE"
           DISPLAY "********************".

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF VALUTAZIONE(I) >= 6 THEN

                   MOVE NOME(I) TO NOME_IDONEO(I)
                   MOVE COGNOME(I) TO COGNOME_IDONEO(I)
                   MOVE VALUTAZIONE(I) TO PUNTEGGIO(I)

               END-IF

           END-PERFORM.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > SIZE-ARRAY

               DISPLAY "NOME:" NOME_IDONEO(J)
                       "COGNOME: " COGNOME_IDONEO(J)
                       "PUNTEGGIO: " PUNTEGGIO(J)

           END-PERFORM.
      *-------------------------- FUNCTION AE --------------------------

       ESAME-ORALE.

           DISPLAY "ESAME ORALE"
           DISPLAY "********************".

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

            COMPUTE VALORE-RANDOM-ORALE = FUNCTION RANDOM * 30

            MOVE NOME_IDONEO(I) TO NOME_IDONEO_GRAD(I)
            MOVE COGNOME_IDONEO(I) TO COGNOME_IDONEO_GRAD(I)
            MOVE PUNTEGGIO(I) TO PUNTEGGIO_SCRITTO(I)
            IF PUNTEGGIO_SCRITTO(I) >= 6
               THEN
                   MOVE VALORE-RANDOM-ORALE TO PUNTEGGIO_ORALE(I)
            END-IF

            PERFORM CALCOLA-MEDIA

           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "NOME:" NOME_IDONEO_GRAD(I)
                       "COGNOME: " COGNOME_IDONEO_GRAD(I)
                       "PUNTEGGIO_SCRITTO: " PUNTEGGIO_SCRITTO(I)
                       " PUNTEGGIO_ORALE: " PUNTEGGIO_ORALE(I)
                       " MEDIA: " MEDIA_VAL(I)

           END-PERFORM.
      *----------------------- FUNCTION AF -----------------------------
       CALCOLA-MEDIA.

           IF PUNTEGGIO_SCRITTO(I) > 0 AND PUNTEGGIO_ORALE(I) > 0 THEN
               COMPUTE MEDIA = (PUNTEGGIO_SCRITTO(I)
               + PUNTEGGIO_ORALE(I))/2
               MOVE MEDIA TO MEDIA_VAL(I)
            END-IF.

      *----------------------- FUNCTION AG -----------------------------

       PROMOSSI.

           DISPLAY "PROMOZIONE".
           DISPLAY "********************".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY - 1

               IF MEDIA_VAL(I) >= 6 THEN

                   DISPLAY "NOME: " NOME_IDONEO_GRAD(I)
                           "COGNOME: " COGNOME_IDONEO_GRAD(I)
                           "MEDIA: " MEDIA_VAL(I)
                           "PROMOSSO"
               END-IF

           END-PERFORM.
