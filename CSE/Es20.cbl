       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  STUDENTI.
           05 NOME_STUDENTE PIC A(10).
           05 CLASSE_STUDENTE PIC X(10).
           05 GIUDIZIO PIC 9(2).

       01 VOTO.
           05 VOTO_MAX PIC 9(2).
           05 VOTO_MIN PIC 9(2).
       01 FLAG.
           05 FILLER-01 PIC 9(1).

       01 SIZE-ARRAY USAGE IS INDEX.

       01  TABELLA_STUDENTI.
           05 LISTA_STUDENTI OCCURS 1 TO 100 TIMES
           DEPENDING ON SIZE-ARRAY
           INDEXED BY I.
               10 NOME PIC A(10).
               10 CLASSE PIC X(10).
               10 GIUDIZIO_STUDENTE PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "INSERISCI LA DIMENSIONE"
            ACCEPT SIZE-ARRAY.

            PERFORM RIEMPI-ARRAY.
            PERFORM STAMPA-ARRAY.
            PERFORM MIGLIOR-STUDENTE.
            PERFORM PEGGIOR-STUDENTE.

            STOP RUN.
      *---------------------------------------- FUNCTION AA ------------
      *INSERISCE UN ELENCO DI STUDENTI
       RIEMPI-ARRAY.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY
               DISPLAY "INSERISCI NOME STUDENTE"
               ACCEPT NOME_STUDENTE
               MOVE NOME_STUDENTE TO NOME(I)
               DISPLAY "INSERISCI CLASSE STUDENTE"
               ACCEPT CLASSE_STUDENTE
               MOVE CLASSE_STUDENTE TO CLASSE(I)
               DISPLAY "INSERISCI IL GIUDIZIO"
               ACCEPT GIUDIZIO
               MOVE GIUDIZIO TO GIUDIZIO_STUDENTE(I)
           END-PERFORM.

      *---------------------------------------- FUNCTION AB ------------
      *STAMPA L'ELENCO
       STAMPA-ARRAY.

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               DISPLAY "NOME: " NOME(I)
                       "CLASSE: " CLASSE(I)
                       "GIUDIZIO: " GIUDIZIO_STUDENTE(I)

           END-PERFORM.
      *---------------------------------------- FUNCTION AC ------------
      *TROVA IL MIGLIOR STUDENTE CON IL MIGLIOR GIUDIZIO
       MIGLIOR-STUDENTE.

           INITIALIZE VOTO_MAX.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF GIUDIZIO_STUDENTE(I) > VOTO_MAX THEN
                   MOVE GIUDIZIO_STUDENTE(I) TO VOTO_MAX
               END-IF

               IF VOTO_MAX = GIUDIZIO_STUDENTE(I) THEN
                   MOVE I TO FILLER-01
               END-IF
           END-PERFORM.

           DISPLAY "STUDENTE CON GIUDIZIO MAX:" VOTO_MAX.
           DISPLAY "NOME: " NOME(FILLER-01).

      *---------------------------------------- FUNCTION AD ------------
       PEGGIOR-STUDENTE.

           MOVE GIUDIZIO_STUDENTE(1) TO VOTO_MIN.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SIZE-ARRAY

               IF GIUDIZIO_STUDENTE(I) < VOTO_MIN THEN
                   MOVE GIUDIZIO_STUDENTE(I) TO VOTO_MIN
               END-IF
               IF VOTO_MIN = GIUDIZIO_STUDENTE(I) THEN
                   MOVE I TO FILLER-01
               END-IF

           END-PERFORM.

           DISPLAY "STUDENTE CON GIUDIZIO MIN:" VOTO_MIN.
           DISPLAY "NOME: " NOME(FILLER-01).
