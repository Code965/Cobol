       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRERIA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 LIBRERIA.
           05 SCAFFALI OCCURS 1 TO 100 TIMES DEPENDING ON SIZE-ARRAY
               INDEXED BY I.
               10 AUTORE PIC A(20).
               10 ARGOMENTO PIC X(50).
               10 PREZZO PIC 9(3).

       01 SIZE-ARRAY USAGE IS INDEX.

       01 ARGOMENTO_RICERCATO PIC X(50).
       01 PREZZO_MAX PIC 9(3).
       01 PREZZO_MIN PIC 9(3).
       01 INDICE-MAX PIC 9(2).
       01 INDICE-MIN PIC 9(2).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "QUANTI LIBRI CI SONO?"
           ACCEPT SIZE-ARRAY.
           PERFORM RIEMPI-LIBRERIA.
           PERFORM ELENCO-LIBRERIA.
           PERFORM RICERCA-ARGOMENTO.

           STOP RUN.

      *------------------------------- FUNCTION AA ---------------------
       RIEMPI-LIBRERIA.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "INSERISCI L'AUTORE"
               ACCEPT AUTORE(I)
               DISPLAY "INSERISCI ARGOMENTO"
               ACCEPT ARGOMENTO(I)
               DISPLAY "INSERISCI PREZZO"
               ACCEPT PREZZO(I)
           END-PERFORM.

      *-------------------------------- FUNCTION AB --------------------
       ELENCO-LIBRERIA.
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "AUTORE: " AUTORE(I)
                       "ARGOMENTO: " ARGOMENTO(I)
                       "PREZZO: " PREZZO(I)

           END-PERFORM.
      *------------------------------ FUNCTION AB ----------------------
       RICERCA-ARGOMENTO.

           DISPLAY "INSERISCI L'ARGOMENTO".
           ACCEPT ARGOMENTO_RICERCATO.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF  ARGOMENTO_RICERCATO = ARGOMENTO(I) THEN
                   DISPLAY "AERG" ARGOMENTO(I)

                   PERFORM PREZZO-VOLUME-MAX
                   PERFORM PREZZO-VOLUME-MIN

               ELSE
                   DISPLAY "ARGOMENTO NON TROVATO"
               END-IF

           END-PERFORM.
      *------------------------------ FUNCTION AC ----------------------
       PREZZO-VOLUME-MAX.

           INITIALIZE PREZZO_MAX.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF PREZZO_MAX < PREZZO(I) THEN

                   MOVE PREZZO(I) TO PREZZO_MAX
                   MOVE I TO INDICE-MAX
               END-IF

           END-PERFORM.

           DISPLAY "AUTORE: " AUTORE(INDICE-MAX)
                   "VOLUME CON PREZZO PIU ALTO: " PREZZO_MAX.

      *----------------------------- FUNCTION AD -----------------------
       PREZZO-VOLUME-MIN.

           MOVE PREZZO(1) TO PREZZO_MIN.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF  PREZZO_MIN > PREZZO(I) THEN

                   MOVE PREZZO( I + 1) TO PREZZO_MIN
                   MOVE I TO INDICE-MIN
               END-IF

           END-PERFORM.

           DISPLAY "AUTORE: " AUTORE(INDICE-MIN)
                   "VOLUME CON PREZZO PIU BASSO: " PREZZO_MIN.
